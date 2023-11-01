#!/usr/bin/env stack
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Foldable
import Data.Function
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Typeable
import GHC.Exts (IsString (..))
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router

type ShutdownHandler = TVar (IO ())

execShutdown :: ShutdownHandler -> IO ()
execShutdown = join . readTVarIO

port :: Port
port = 12345

-- seconds
shutdownTime :: Int
shutdownTime = 20

oneSecond :: Int
oneSecond = 1000 * 1000

main :: IO ()
main = do
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
       )

apiHandler :: Server API
apiHandler = apiA :<|> apiB :<|> apiC
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
    & setHost "localhost"
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
    Delayed env (Server (MyPath path :> api)) ->
    Router env
  route _ ctx server = StaticRouter (M.fromList [(T.pack pathName, inner)]) []
   where
    pathName = asString @path Proxy
    inner = route (Proxy @api) ctx server
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

-- ============================== Capture ==============================

-- | Capture
data MyCapture a

instance (HasServer api ctx, FromHttpApiData a, Typeable a) => HasServer (MyCapture a :> api) ctx where
  type ServerT (MyCapture a :> api) m = a -> ServerT api m

  route ::
    Proxy (MyCapture a :> api) ->
    Context ctx ->
    Delayed env (Server (MyCapture a :> api)) ->
    Router env
  route _ ctx delayedHandler = routed
   where
    captureHint = CaptureHint "x" (typeRep (Proxy @a)) -- used to show capture in layout
    routed = CaptureRouter [captureHint] $ route (Proxy @api) ctx next
    next = addCapture delayedHandler $ \capture -> case parseUrlPiece capture of
      Right a -> return a
      Left err -> delayedFail $ err400{errReasonPhrase = T.unpack ("Failed to parse capture /:" <> capture <> "\n" <> err)}
  hoistServerWithContext _ ctx f serverHandler = hoistServerWithContext (Proxy @api) ctx f . serverHandler

-- ============================== Verb ==============================
--

data MyQueryParam (name :: Symbol) a

instance (HasServer api ctx, KnownSymbol name, FromHttpApiData a) => HasServer (MyQueryParam name a :> api) ctx where
  type ServerT (MyQueryParam name a :> api) m = a -> ServerT api m

  route ::
    Proxy (MyQueryParam name a :> api) ->
    Context ctx ->
    Delayed env (Server (MyQueryParam name a :> api)) ->
    Router env
  route _ ctx delayedHandler = route (Proxy @api) ctx delayedHandler'
   where
    name = symbolVal (Proxy @name)
    nameB = encodeUtf8 $ T.pack name
    delayedHandler' = addParameterCheck delayedHandler $ do
      req <- ask
      let params = queryString req
      case find ((== nameB) . fst) params of
        Just (_, Just rawValue) -> case parseQueryParam (decodeUtf8 rawValue) of
          Right parsed -> return parsed
          Left parseError -> delayedFail $ err400{errReasonPhrase = "Failed to ?" <> name <> ":\n" <> T.unpack parseError}
        Just (_, Nothing) -> delayedFail $ err400{errReasonPhrase = "?" <> name <> " requires a value"}
        _ -> delayedFail $ err400{errReasonPhrase = "?" <> name <> "=<value> is required"}

  hoistServerWithContext _ ctx f handler = hoistServerWithContext (Proxy @api) ctx f . handler

-- ============================== Verb ==============================
--

-- | Verb
data MyVerbName = MyGET | MyPOST

type MyPost = MyVerb 'MyPOST

data MyVerb (name :: MyVerbName) a

type MyGet = MyVerb 'MyGET

instance (Show a, StringRepresentable name) => HasServer (MyVerb name a) ctx where
  type ServerT (MyVerb name a) handler = handler a
  route ::
    forall env.
    Proxy (MyVerb name a) ->
    Context ctx ->
    Delayed env (Server (MyVerb name a)) ->
    Router env
  route _ _ server = StaticRouter M.empty [processRequest]
   where
    method = asString $ Proxy @name
    processRequest :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
    processRequest env req k = runAction act env req k $ \a -> do
      Route $ responseLBS status200 [header] $ B.pack $ show a
     where
      header = (hAccept, "text/plain")
      act = server `addMethodCheck` validateMethodName (requestMethod req) method

  hoistServerWithContext :: Proxy (MyVerb name a) -> Proxy ctx -> (forall x. m x -> n x) -> m a -> n a
  hoistServerWithContext _ _ f = f

validateMethodName :: Method -> Method -> DelayedIO ()
validateMethodName expected method =
  if expected == method
    then return ()
    else delayedFail $ err400{errReasonPhrase = "Unknown method" <> show method}

-- data A b where
--   A :: {myAValue :: Int -> b, myARun :: b -> c} -> A c

-- aa :: A String
-- aa = A (\(x :: Int) -> x) (show)

-- runA :: Int -> A b -> b
-- runA x A {..} = myARun $ myAValue x
