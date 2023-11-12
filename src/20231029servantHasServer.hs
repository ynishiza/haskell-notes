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

module Note20231029servantHasServer where

import Control.Applicative
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
import GHC.Exts (IsString (..))
import GHC.TypeLits
import Network.HTTP.Client (defaultManagerSettings, newManager)
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

type ShutdownHandler = TVar (IO ())

data MyVerbName = MyGET | MyPOST

genSingletons [''MyVerbName]

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

type API =
  MyPath "api"
    :> ( MyPath "a" :> (MyGet PlainText String :<|> MyPost PlainText ())
          :<|> (MyPath "b" :> MyCapture Int :> MyGet PlainText Int)
          :<|> (MyPath "c" :> MyQueryParam "name" String :> MyQueryParam "id" Int :> MyGet PlainText String)
          :<|> (MyPath "d" :> MyEmpty)
       )

instance {-# OVERLAPS #-} (Show a) => MimeRender PlainText a where
  mimeRender _ = B.fromStrict . encodeUtf8 . T.pack . show

instance {-# OVERLAPS #-} (Read a) => MimeUnrender PlainText a where
  mimeUnrender _ value = case (reads @a) text of
    [(v, _)] -> Right v
    _ -> Left $ "Failed to decode: " <> text
   where
    text = T.unpack $ decodeUtf8 $ B.toStrict value

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

-- ============================== Setup ==============================

type StringRepresentable n = KnownSymbol (AsSymbol n)

type AsSymbol :: a -> Symbol
type family AsSymbol n :: Symbol

type instance AsSymbol MyGET = "GET"

type instance AsSymbol MyPOST = "POST"

type instance AsSymbol (a :: Symbol) = a

asString :: forall name s. (IsString s, StringRepresentable name) => Proxy name -> s
asString _ = fromString $ symbolVal $ Proxy @(AsSymbol name)

-- ============================== Path ==============================
--

-- * Servers

-- ** 'MyPath'

data MyPath (path :: Symbol)

instance (HasServer api ctx, KnownSymbol path) => HasServer (MyPath path :> api) ctx where
  type ServerT (MyPath path :> api) m = ServerT api m

  route ::
    Proxy (MyPath path :> api) ->
    Context ctx ->
    Delayed env (Server api) ->
    Router env
  route _ ctx delayedHandler = StaticRouter (M.fromList [(T.pack pathName, inner)]) []
   where
    pathName = asString @path Proxy
    inner = route (Proxy @api) ctx delayedHandler
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance (KnownSymbol path, HasClient m api) => HasClient m (MyPath path :> api) where
  type Client m (MyPath path :> api) = Client m api
  clientWithRoute _ _ req = clientWithRoute (Proxy @m) (Proxy @api) $ appendToPath (asString (Proxy @path)) req
  hoistClientMonad _ _ = hoistClientMonad (Proxy @m) (Proxy @api)

-- ============================== Capture ==============================

-- ** Capture

-- | Capture
data MyCapture (a :: Type)

instance (HasServer api ctx, FromHttpApiData a, Typeable a) => HasServer (MyCapture a :> api) ctx where
  type ServerT (MyCapture a :> api) m = a -> ServerT api m

  route ::
    forall env.
    Proxy (MyCapture a :> api) ->
    Context ctx ->
    Delayed env (a -> Server api) ->
    Router env
  route _ ctx handler = CaptureRouter [captureHint] $ route (Proxy @api) ctx (handler' handler)
   where
    captureHint = CaptureHint "x" (typeRep (Proxy @a)) -- used to show capture in layout
    handler' :: Delayed env (a -> Server api) -> Delayed (Text, env) (Server api)
    handler' Delayed{..} =
      Delayed
        { capturesD = \(text, env) -> (,) <$> parseCapture text <*> capturesD env
        , serverD = \(a, caps) params headers auth body req -> ($ a) <$> serverD caps params headers auth body req
        , ..
        }
    handler'' = addCapture handler parseCapture -- same
    parseCapture text = case parseUrlPiece text of
      Right a -> return a
      Left err -> delayedFail $ err400{errReasonPhrase = T.unpack ("Failed to parse capture /:" <> text <> "\n" <> err)}
  hoistServerWithContext _ ctx f serverHandler = hoistServerWithContext (Proxy @api) ctx f . serverHandler

instance (ToHttpApiData a, HasClient m api) => HasClient m (MyCapture a :> api) where
  type Client m (MyCapture a :> api) = a -> Client m api
  clientWithRoute _ _ req value = clientWithRoute (Proxy @m) (Proxy @api) $ appendToPath (toUrlPiece value) req
  hoistClientMonad _ _ f = (hoistClientMonad (Proxy @m) (Proxy @api) f <$>)

-- ============================== Query param ==============================
--

-- ** Query param

data MyQueryParam (name :: Symbol) (a :: Type)

instance (HasServer api ctx, KnownSymbol name, FromHttpApiData a) => HasServer (MyQueryParam name a :> api) ctx where
  type ServerT (MyQueryParam name a :> api) m = a -> ServerT api m

  route ::
    Proxy (MyQueryParam name a :> api) ->
    Context ctx ->
    Delayed env (a -> Server api) ->
    Router env
  route _ ctx delayedHandler = route (Proxy @api) ctx (handler' delayedHandler)
   where
    name = asString (Proxy @name)
    nameB = encodeUtf8 $ asString (Proxy @name)
    handler' :: Delayed env (a -> Server api) -> Delayed env (Server api)
    handler' Delayed{..} =
      Delayed
        { paramsD = (,) <$> parseParam <*> paramsD
        , serverD = \cap (a, params) headers auth body req -> ($ a) <$> serverD cap params headers auth body req
        , ..
        }
    handler'' = addParameterCheck delayedHandler parseParam -- same
    parseParam = do
      req <- ask
      let params = queryString req
      case find ((== nameB) . fst) params of
        Just (_, Just rawValue) -> case parseQueryParam (decodeUtf8 rawValue) of
          Right parsed -> return parsed
          Left parseError -> delayedFail $ err400{errReasonPhrase = "Failed to ?" <> name <> ":\n" <> T.unpack parseError}
        Just (_, Nothing) -> delayedFail $ err400{errReasonPhrase = "?" <> name <> " requires a value"}
        _ -> delayedFail $ err400{errReasonPhrase = "?" <> name <> "=<value> is required"}

  hoistServerWithContext _ ctx f handler = hoistServerWithContext (Proxy @api) ctx f . handler

instance (ToHttpApiData a, HasClient m api, KnownSymbol name) => HasClient m (MyQueryParam name a :> api) where
  type Client m (MyQueryParam name a :> api) = a -> Client m api
  clientWithRoute _ _ req value = clientWithRoute (Proxy @m) (Proxy @api) $ appendToQueryString (asString (Proxy @name)) (Just bytes) req
   where
    bytes = encodeUtf8 $ toQueryParam value
  hoistClientMonad _ _ f = (hoistClientMonad (Proxy @m) (Proxy @api) f <$>)

-- ============================== Verb ==============================
--

-- ** Verb

-- | Verb
type MyPost = MyVerb 'MyPOST 201

data MyVerb (name :: MyVerbName) (status :: Nat) contentType (a :: Type)

type MyGet = MyVerb 'MyGET 200

instance (MimeRender contentType a, StringRepresentable name, KnownNat status) => HasServer (MyVerb name status contentType a) ctx where
  type ServerT (MyVerb name status contentType a) handler = handler a
  route ::
    forall env.
    Proxy (MyVerb name status contentType a) ->
    Context ctx ->
    Delayed env (Handler a) ->
    Router env
  route _ _ handler = StaticRouter M.empty [handleRequest]
   where
    method = asString $ Proxy @name
    status = statusFromNat (Proxy @status)
    handleRequest :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
    handleRequest env req k = runAction (handler' handler) env req k $ \a -> do
      Route $ responseLBS status [header] $ mimeRender (Proxy @contentType) a
     where
      header = (hAccept, "text/plain")
      handler' Delayed{..} =
        Delayed
          { methodD = methodD >> validateMethodName (requestMethod req) method
          , ..
          }
      handler'' = handler `addMethodCheck` validateMethodName (requestMethod req) method -- same

  hoistServerWithContext :: Proxy (MyVerb name status contentType a) -> Proxy ctx -> (forall x. m x -> n x) -> m a -> n a
  hoistServerWithContext _ _ f = f

instance (MimeUnrender contentType a, SingI (name :: MyVerbName), RunClient m) => HasClient m (MyVerb name status contentType a) where
  type Client m (MyVerb name status contentType a) = m a
  clientWithRoute _ _ req = C.runRequestAcceptStatus Nothing req' >>= parseResponse
   where
    parseResponse :: C.Response -> m a
    parseResponse res
      | statusCode (C.responseStatusCode res) >= 300 = undefined
      | result <- mimeUnrender (Proxy @contentType) (C.responseBody res) = case result of
          Right v -> return v
          Left message -> C.throwClientError $ DecodeFailure (T.pack message) res
    req' :: C.Request
    req' =
      req
        { C.requestMethod = case (sing @name) of
            SMyGET -> methodGet
            SMyPOST -> methodPost
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
  route _ _ delayedHandler =
    StaticRouter
      M.empty
      [ \env req k -> runAction delayedHandler env req k (\_ -> Fail $ err500{errReasonPhrase = "EMPTY SERVER"})
      ]
  hoistServerWithContext _ _ f = f

instance (RunClient m) => HasClient m MyEmpty where
  type Client m MyEmpty = m ()
  clientWithRoute _ _ req = void $ C.runRequestAcceptStatus Nothing req
  hoistClientMonad _ _ f = f

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
