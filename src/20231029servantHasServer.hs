#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Note: define a module to generate Haddock documentation per note
module Note20231029servantHasServer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.ByteString.Char8 qualified as B
import Data.Foldable
import Data.Function
import Data.Kind
import Data.Map qualified as M
import Data.Singletons.TH
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Typeable
import GHC.TypeLits
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Media (renderHeader)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Status
import Servant.Client hiding (Response)
import Servant.Client.Core (RunClient, appendToPath, appendToQueryString)
import Servant.Client.Core qualified as C
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import Test.Hspec

-- ============================== Setup ==============================

data MyVerbName = MyGETName | MyPOSTName

-- Singleton for converting type to value
genSingletons [''MyVerbName]

instance {-# OVERLAPPABLE #-} (Show a) => MimeRender PlainText a where
  mimeRender _ = B.fromStrict . encodeUtf8 . T.pack . show

instance {-# OVERLAPPABLE #-} (Read a) => MimeUnrender PlainText a where
  mimeUnrender _ value = case (reads @a) text of
    [(v, _)] -> Right v
    _ -> Left $ "Failed to decode: " <> text
   where
    text = T.unpack $ decodeUtf8 $ B.toStrict value

-- ============================== API ==============================
--

-- * API

type API =
  MyPath "api"
    :> ( MyPath "a" :> (MyGETAPI PlainText String :<|> MyPOSTAPI PlainText ())
          :<|> (MyPath "b" :> MyCapture "name" Int :> MyGETAPI PlainText Int)
          :<|> (MyPath "c" :> MyQueryParam "name" String :> MyQueryParam "id" Int :> MyGETAPI PlainText String)
          :<|> (MyPath "d" :> MyEmpty)
       )

apiHandler :: Server API
apiHandler = apiA :<|> apiB :<|> apiC :<|> emptyHandler
 where
  apiA =
    return "a hello"
      :<|> do
        liftIO $ putStrLn "POST called"
        return ()
  apiB x = return (x * x)
  apiC name idNumber = do
    return $ "Received:" <> show (name, idNumber)

-- ============================== Path ==============================
--

-- * Servers

-- ** Path processor

data MyPath (path :: Symbol)

{- |
> type ServerT (MyPath path :> api) m = ServerT api m
-}
instance (HasServer api ctx, KnownSymbol path) => HasServer (MyPath path :> api) ctx where
  type ServerT (MyPath path :> api) m = ServerT api m

  route ::
    Proxy (MyPath path :> api) ->
    Context ctx ->
    Delayed env (Server api) ->
    Router env
  route _ ctx delayedRouter = StaticRouter (M.fromList [(T.pack pathName, routeNext)]) []
   where
    pathName = symbolVal @path Proxy
    routeNext = route (Proxy @api) ctx delayedRouter
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

{- |
> ClientM (MyPath path :> api) m = ClientM api m
-}
instance (KnownSymbol path, HasClient m api) => HasClient m (MyPath path :> api) where
  type Client m (MyPath path :> api) = Client m api
  clientWithRoute _ _ req = clientWithRoute (Proxy @m) (Proxy @api) $ appendToPath (T.pack $ symbolVal (Proxy @path)) req
  hoistClientMonad _ _ = hoistClientMonad (Proxy @m) (Proxy @api)

-- ============================== Capture ==============================

-- ** Capture processor

--
data MyCapture (name :: Symbol) (a :: Type)

{- |
> ServerT (MyCapture a :> api) m = a -> ServerT api m
-}
instance (HasServer api ctx, FromHttpApiData a, KnownSymbol name, Typeable a) => HasServer (MyCapture name a :> api) ctx where
  type ServerT (MyCapture name a :> api) m = a -> ServerT api m

  route ::
    forall env.
    Proxy (MyCapture name a :> api) ->
    Context ctx ->
    Delayed env (a -> Server api) ->
    Router env
  route _ ctx delayedRouter = CaptureRouter [captureHint] $ route (Proxy @api) ctx delayedRouter'
   where
    captureHint = CaptureHint (T.pack $ symbolVal (Proxy @name)) (typeRep (Proxy @a)) -- used to show capture in layout
    routeNext :: Delayed env (a -> Server api) -> Delayed (Text, env) (Server api)
    routeNext Delayed{..} =
      Delayed
        { capturesD = \(text, env) ->
            (,) <$> parseCapture text <*> capturesD env
        , serverD = \(a, caps) params headers auth body req ->
            ($ a) <$> serverD caps params headers auth body req
        , ..
        }
    delayedRouter' = routeNext delayedRouter
    delayedRouter'' = addCapture delayedRouter parseCapture -- same
    parseCapture text = case parseUrlPiece text of
      Right a -> return a
      Left err -> delayedFail $ err400{errReasonPhrase = T.unpack ("Failed to parse capture /:" <> text <> "\n" <> err)}
  hoistServerWithContext _ ctx f serverHandler = hoistServerWithContext (Proxy @api) ctx f . serverHandler

{- |
> ClientM m (MyCapture a :> api) = a -> ClientM m api
-}
instance (ToHttpApiData a, HasClient m api) => HasClient m (MyCapture name a :> api) where
  type Client m (MyCapture name a :> api) = a -> Client m api
  clientWithRoute _ _ req value = clientWithRoute (Proxy @m) (Proxy @api) $ appendToPath (toUrlPiece value) req
  hoistClientMonad _ _ f = (hoistClientMonad (Proxy @m) (Proxy @api) f <$>)

-- ============================== Query param ==============================
--

-- ** Query param

data MyQueryParam (name :: Symbol) (a :: Type)

{- |
> type ServerT (MyQueryParam name a :> api) m = a -> ServerT api m
-}
instance (HasServer api ctx, KnownSymbol name, FromHttpApiData a) => HasServer (MyQueryParam name a :> api) ctx where
  type ServerT (MyQueryParam name a :> api) m = a -> ServerT api m

  route ::
    Proxy (MyQueryParam name a :> api) ->
    Context ctx ->
    Delayed env (a -> Server api) ->
    Router env
  route _ ctx delayedRouter = route (Proxy @api) ctx delayedRouter'
   where
    name = symbolVal (Proxy @name)
    routeNext :: Delayed env (a -> Server api) -> Delayed env (Server api)
    routeNext Delayed{..} =
      Delayed
        { paramsD =
            (,) <$> (ask >>= parseQueryParamFromRequest @a name) <*> paramsD
        , serverD = \cap (a, params) headers auth body req ->
            ($ a) <$> serverD cap params headers auth body req
        , ..
        }
    delayedRouter' = routeNext delayedRouter
    delayedRouter'' = addParameterCheck delayedRouter (ask >>= parseQueryParamFromRequest @a name) -- same as routeNext delayedRouter

  hoistServerWithContext _ ctx f handler = hoistServerWithContext (Proxy @api) ctx f . handler

parseQueryParamFromRequest :: (FromHttpApiData a) => String -> Request -> DelayedIO a
parseQueryParamFromRequest name req = do
  let params = queryString req
  case find ((== nameB) . fst) params of
    Just (_, Just rawValue) -> case parseQueryParam (decodeUtf8 rawValue) of
      Right parsed -> return parsed
      Left parseError -> delayedFail $ err400{errReasonPhrase = "Failed to ?" <> name <> ":\n" <> T.unpack parseError}
    Just (_, Nothing) -> delayedFail $ err400{errReasonPhrase = "?" <> name <> " requires a value"}
    _ -> delayedFail $ err400{errReasonPhrase = "?" <> name <> "=<value> is required"}
 where
  nameB = encodeUtf8 $ T.pack name

{- |
> type Client m (MyQueryParam name a :> api) = a -> Client m api
-}
instance (ToHttpApiData a, HasClient m api, KnownSymbol name) => HasClient m (MyQueryParam name a :> api) where
  type Client m (MyQueryParam name a :> api) = a -> Client m api
  clientWithRoute _ _ req value =
    clientWithRoute (Proxy @m) (Proxy @api)
      $ appendToQueryString (T.pack $ symbolVal (Proxy @name)) (Just bytes) req
   where
    bytes = encodeUtf8 $ toQueryParam value
  hoistClientMonad _ _ f = (hoistClientMonad (Proxy @m) (Proxy @api) f <$>)

-- ============================== Verb ==============================
--

-- ** Verb

-- | Verb
type MyPOSTAPI = MyVerb 'MyPOSTName 201

data MyVerb (name :: MyVerbName) (status :: Nat) contentType (a :: Type)

type MyGETAPI = MyVerb 'MyGETName 200

{- |
> type ServerT (MyVerb name status contentType a) m = m a
-}
instance (MimeRender contentType a, SingI name, KnownNat status) => HasServer (MyVerb name status contentType a) ctx where
  type ServerT (MyVerb name status contentType a) m = m a
  route ::
    forall env.
    Proxy (MyVerb name status contentType a) ->
    Context ctx ->
    Delayed env (Handler a) ->
    Router env
  route _ _ delayedRouter = StaticRouter M.empty [handleRequest]
   where
    method = case (sing @name) of
      SMyGETName -> "GET"
      SMyPOSTName -> "POST"
    status = statusFromNat (Proxy @status)
    handleRequest :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
    handleRequest env req k = runAction delayedRouter' env req k $ \a -> do
      Route $ responseLBS status [header] $ mimeRender (Proxy @contentType) a
     where
      header = (hAccept, renderHeader $ contentType (Proxy @contentType))
      routeNext Delayed{..} =
        Delayed
          { methodD = methodD >> validateMethodName (requestMethod req) method
          , ..
          }
      delayedRouter' = routeNext delayedRouter
      delayedRouter'' = delayedRouter `addMethodCheck` validateMethodName (requestMethod req) method -- same

  hoistServerWithContext :: Proxy (MyVerb name status contentType a) -> Proxy ctx -> (forall x. m x -> n x) -> m a -> n a
  hoistServerWithContext _ _ f = f

{- |
> type Client m (MyVerb name status contentType a) = m a
-}
instance (MimeUnrender contentType a, SingI (name :: MyVerbName), RunClient m) => HasClient m (MyVerb name status contentType a) where
  type Client m (MyVerb name status contentType a) = m a
  clientWithRoute _ _ req = C.runRequestAcceptStatus Nothing req' >>= parseResponse
   where
    parseResponse :: C.Response -> m a
    parseResponse res = case mimeUnrender (Proxy @contentType) (C.responseBody res) of
      Right v -> return v
      Left message -> C.throwClientError $ DecodeFailure (T.pack message) res
    req' :: C.Request
    req' =
      req
        { C.requestMethod = case (sing @name) of
            SMyGETName -> methodGet
            SMyPOSTName -> methodPost
        }
  hoistClientMonad _ _ f = f

validateMethodName :: Method -> Method -> DelayedIO ()
validateMethodName expected method =
  if expected == method
    then return ()
    else delayedFail $ err400{errReasonPhrase = "Unknown method" <> show method}

-- ============================== Empty ==============================
--

-- * Empty

data MyEmpty

emptyHandler :: (Monad m) => ServerT MyEmpty m
emptyHandler = return ()

instance HasServer MyEmpty ctx where
  type ServerT MyEmpty m = m ()
  route _ _ delayedRouter =
    StaticRouter
      M.empty
      [ \env req k ->
          runAction delayedRouter env req k 
            (\_ -> Fail $ err500{errReasonPhrase = "EMPTY SERVER"})
      ]
  hoistServerWithContext _ _ f = f

instance (RunClient m) => HasClient m MyEmpty where
  type Client m MyEmpty = m ()
  clientWithRoute _ _ req = void $ C.runRequestAcceptStatus Nothing req
  hoistClientMonad _ _ f = f

-- ============================== Server ==============================

type ShutdownHandler = TVar (IO ())

execShutdown :: ShutdownHandler -> IO ()
execShutdown = join . readTVarIO

port :: Port
port = 12345

host :: HostPreference
host = "localhost"

testUrl :: BaseUrl
testUrl = BaseUrl Http "localhost" port ""

-- seconds
shutdownTime :: Int
shutdownTime = 20

oneSecond :: Int
oneSecond = 1000 * 1000

main :: IO ()
main = do
  hspec spec

startTestServer :: IO ()
startTestServer = do
  shutdownVar <- newTVarIO (pure ())
  let settings = createSettings shutdownVar
      apiLayout = layout (Proxy @API)
  T.putStrLn apiLayout
  _ <- forkIO $ startServer settings
  threadDelay $ 1 * oneSecond
  putStrLn $ "Shutting down in " <> show shutdownTime <> "s"
  threadDelay $ shutdownTime * oneSecond
  execShutdown shutdownVar
  putStrLn "Shutdown!"

createSettings :: ShutdownHandler -> Settings
createSettings shutdownVar =
  defaultSettings
    & setPort port
    & setHost host
    & setInstallShutdownHandler (atomically . writeTVar shutdownVar)
    & setGracefulShutdownTimeout (Just 1)

startServer :: Settings -> IO ()
startServer settings = do
  putStrLn $ "Listening on localhost:" <> show port
  serveWithContext (Proxy @API) (errorHandlers :. EmptyContext) apiHandler
    & runSettings settings

errorHandlers :: ErrorFormatters
errorHandlers = defaultErrorFormatters

-- ============================== Test ==============================
--
--

-- * Test

-- TODO
callAGET :: ClientM String
callAPOST :: ClientM ()
callB :: Int -> ClientM Int
callC :: String -> Int -> ClientM String
callD :: ClientM ()
(callAGET :<|> callAPOST) :<|> (callB :<|> callC :<|> callD) = client (Proxy @API)

spec :: Spec
spec =
  ( \runSpec -> do
      manager <- newManager defaultManagerSettings
      shutdownVar :: ShutdownHandler <- newTVarIO (pure ())
      let
        env = mkClientEnv manager testUrl
        info = layout (Proxy @API)
      void $ forkIO $ startServer $ createSettings shutdownVar
      T.putStrLn info
      threadDelay oneSecond
      runSpec env
      execShutdown shutdownVar
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith ClientEnv
baseSpec = describe "API" $ do
  let
    expectSuccess :: ClientEnv -> ClientM a -> (a -> Expectation) -> Expectation
    expectSuccess env c test = do
      result <- runClientM c env
      case result of
        Left e -> expectationFailure $ "ERROR: " <> show e
        Right v -> test v
    expectError :: (Show a) => ClientEnv -> ClientM a -> (ClientError -> Expectation) -> Expectation
    expectError env c test = do
      result <- runClientM c env
      case result of
        Left e -> test e
        Right v -> expectationFailure $ "Expected error but received response" <> show v

  it "GET /api/a" $ \env -> do
    expectSuccess env callAGET (`shouldBe` "a hello")

  it "POST /api/a" $ \env -> do
    expectSuccess env callAPOST (`shouldBe` ())

  it "GET /api/b" $ \env -> do
    expectSuccess env (callB 2) (`shouldBe` 4)

  it "GET /api/c" $ \env -> do
    expectSuccess env (callC "ABC" 10) (`shouldBe` "Received:(\"ABC\",10)")

  it "GET /api/d" $ \env -> do
    expectError env callD $ \case
      (FailureResponse _ res) -> C.responseStatusCode res `shouldBe` Status 500 "EMPTY SERVER"
      e -> expectationFailure $ "Unexpected error:" <> show e
