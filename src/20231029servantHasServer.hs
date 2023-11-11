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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Note20231029servantHasServer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe (throwM)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Foldable
import Data.Function
import Data.Kind
import Data.Map qualified as M
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
import Servant.Client.Core (RunClient)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import Test.Hspec

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
    :> ( MyPath "a" :> (MyGet String :<|> MyPost ())
          :<|> (MyPath "b" :> MyCapture Int :> MyGet Int)
          :<|> (MyPath "c" :> MyQueryParam "name" String :> MyQueryParam "id" Int :> MyGet String)
          :<|> (MyPath "d" :> MyEmpty)
       )

apiHandler :: Server API
apiHandler = apiA :<|> apiB :<|> apiC :<|> emptyHandler
 where
  apiA =
    return "a hello"
      :<|> do
        liftIO $ putStrLn "POST"
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

instance (HasClient m api) => HasClient m (MyPath path :> api) where
  type Client m (MyPath path :> api) = Client m api

-- ============================== Capture ==============================

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

instance (HasClient m api) => HasClient m (MyCapture a :> api) where
  type Client m (MyCapture a :> api) = a -> Client m api

-- ============================== Verb ==============================
--

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
    name = symbolVal (Proxy @name)
    nameB = encodeUtf8 $ T.pack name
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

instance (HasClient m api) => HasClient m (MyQueryParam name a :> api) where
  type Client m (MyQueryParam name a :> api) = a -> Client m api

-- ============================== Verb ==============================
--

-- | Verb
data MyVerbName = MyGET | MyPOST

type MyPost = MyVerb 'MyPOST 201

data MyVerb (name :: MyVerbName) (status :: Nat) (a :: Type)

type MyGet = MyVerb 'MyGET 200

instance (Show a, StringRepresentable name, KnownNat status) => HasServer (MyVerb name status a) ctx where
  type ServerT (MyVerb name status a) handler = handler a
  route ::
    forall env.
    Proxy (MyVerb name status a) ->
    Context ctx ->
    Delayed env (Handler a) ->
    Router env
  route _ _ handler = StaticRouter M.empty [handleRequest]
   where
    method = asString $ Proxy @name
    status = statusFromNat (Proxy @status)
    handleRequest :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
    handleRequest env req k = runAction (handler' handler) env req k $ \a -> do
      Route $ responseLBS status [header] $ B.pack $ show a
     where
      header = (hAccept, "text/plain")
      handler' Delayed{..} =
        Delayed
          { methodD = methodD >> validateMethodName (requestMethod req) method
          , ..
          }
      handler'' = handler `addMethodCheck` validateMethodName (requestMethod req) method -- same

  hoistServerWithContext :: Proxy (MyVerb name status a) -> Proxy ctx -> (forall x. m x -> n x) -> m a -> n a
  hoistServerWithContext _ _ f = f

instance (RunClient m) => HasClient m (MyVerb name status a) where
  type Client m (MyVerb name status a) = m a

validateMethodName :: Method -> Method -> DelayedIO ()
validateMethodName expected method =
  if expected == method
    then return ()
    else delayedFail $ err400{errReasonPhrase = "Unknown method" <> show method}

-- ============================== Verb ==============================
--

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

-- ============================== Test ==============================
--
--

-- TODO
(callA :<|> callA2) :<|> (callB :<|> callC :<|> callD) = client (Proxy @API)

spec :: Spec
spec =
  ( \runSpec -> do
      manager <- newManager defaultManagerSettings
      shutdownVar :: ShutdownHandler <- newTVarIO (pure ())
      let
        env = mkClientEnv manager testUrl
      void $ forkIO $ startServer $ createSettings shutdownVar
      threadDelay oneSecond
      runSpec env
      execShutdown shutdownVar
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith ClientEnv
baseSpec = describe "Main" $ do
  let
    test :: ClientEnv -> ClientM a -> IO a
    test env c = do
      result <- runClientM c env
      case result of
        Left e -> throwM e
        Right v -> return v

  it "" $ \env -> do
    pending
