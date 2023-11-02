{-# LANGUAGE AllowAmbiguousTypes #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

  Source: https://www.well-typed.com/blog/2015/12/dependently-typed-servers/
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception.Safe (throw)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Char (toLower, toUpper)
import Data.Data
import Data.Function
import Data.Kind
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client hiding (Response)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import Test.Hspec

type ShutdownHandler = TVar (IO ())

oneSecond :: Int
oneSecond = 1000 * 1000

shutdownServer :: ShutdownHandler -> IO ()
shutdownServer = join . readTVarIO

testHost :: HostPreference
testHost = "localhost"

testPort :: Port
testPort = 12345

testUrl :: BaseUrl
testUrl = BaseUrl Http "localhost" 12345 ""

main :: IO ()
main = do
  hspec spec

runTestServer :: IO ()
runTestServer = do
  shutdownVar :: ShutdownHandler <- newTVarIO (pure ())
  let serverInfo = layout api
  _ <- forkIO $ do
    threadDelay oneSecond
    putStrLn "Shutting down in 10s"
    threadDelay $ 30 * oneSecond
    shutdownServer shutdownVar
  T.putStrLn serverInfo
  startServer $ serverSettings shutdownVar
  putStrLn "Shutdown"

serverSettings :: ShutdownHandler -> Settings
serverSettings shutdownVar =
  defaultSettings
    & setPort testPort
    & setHost testHost
    & setInstallShutdownHandler (atomically . writeTVar shutdownVar)

startServer :: Settings -> IO ()
startServer settings =
  serve api handler
    & runSettings settings

type API =
  "api"
    :> Summary "Sample"
    :> ( "hello"
          :> Get '[JSON] String
          :<|> DependentValue
       )

api :: Proxy API
api = Proxy

handler :: Server API
handler = return "hello" :<|> dp
 where
  dp :: ServerT DependentValue Handler
  dp = DependentServer $ \v op -> return $ applyOpertion op v

data Dict (c :: Constraint) where
  Dict :: (c) => Dict c

useDict :: Dict c -> ((c) => a) -> a
useDict Dict x = x

-- type clientAPI = "api" :> (Capture' '[Required] "" String :> Capture' '[Required] "" String :> Get '[JSON] String)
type ClientAPI =
  "api"
    :> ( Capture' '[Required] "value" String :> Capture' '[Required] "operation" String :> Get '[JSON] String
          :<|> Capture' '[Required] "value" Int :> Capture' '[Required] "operation" String :> Get '[JSON] Int
       )

stringValue :: String -> String -> ClientM String
intValue :: Int -> String -> ClientM Int
stringValue :<|> intValue = client (Proxy @ClientAPI)

spec :: Spec
spec =
  ( \run -> do
      manager <- newManager defaultManagerSettings
      shutdownVar :: ShutdownHandler <- newTVarIO (pure ())
      let
        env = mkClientEnv manager testUrl
      void $ forkIO $ startServer $ serverSettings shutdownVar
      threadDelay oneSecond
      run env
      shutdownServer shutdownVar
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith ClientEnv
baseSpec = describe "" $ do
  let
    test :: ClientEnv -> ClientM a -> IO a
    test env c = do
      result <- runClientM c env
      case result of
        Left e -> throwM e
        Right v -> return v

  it "GET /api/<string>/upper" $ \run -> do
    x <- test run $ stringValue "abc" "upper"
    x `shouldBe` "ABC"

  it "GET /api/<int>/negate" $ \run -> do
    x <- test run $ intValue 123 "negate"
    x `shouldBe` -123

-- ========================================  Value ========================================
--

data Value a where
  ValueString :: String -> Value String
  ValueInt :: Int -> Value Int

data Some1 f = forall a. Some1 (f a)

instance Show (Value a) where
  show (ValueString v) = "StringValue " <> v
  show (ValueInt v) = "ValueInt " <> show v

data Operation a where
  OpDouble :: Operation Int
  OpNegate :: Operation Int
  OpUpper :: Operation String
  OpLower :: Operation String

applyOpertion :: Operation a -> Value a -> Value a
applyOpertion OpDouble (ValueInt v) = ValueInt $ 2 * v
applyOpertion OpNegate (ValueInt v) = ValueInt $ negate v
applyOpertion OpUpper (ValueString v) = ValueString $ toUpper <$> v
applyOpertion OpLower (ValueString v) = ValueString $ toLower <$> v

instance FromHttpApiData (Operation Int) where
  parseUrlPiece "double" = Right OpDouble
  parseUrlPiece "negate" = Right OpNegate
  parseUrlPiece name = Left $ "Unsupported operation " <> name
instance FromHttpApiData (Operation String) where
  parseUrlPiece "upper" = Right OpUpper
  parseUrlPiece "lower" = Right OpLower
  parseUrlPiece name = Left $ "Unsupported operation " <> name

instance FromHttpApiData (Value Int) where
  parseUrlPiece ((reads @Int) . T.unpack -> [(v, _)]) = Right $ ValueInt v
  parseUrlPiece text = Left $ "Failed to parse Value Int from " <> text

instance FromHttpApiData (Value String) where
  parseUrlPiece text = Right $ ValueString $ T.unpack text

instance MimeRender JSON (Value a) where
  mimeRender p (ValueInt v) = mimeRender p v
  mimeRender p (ValueString v) = mimeRender p v

instance MimeRender JSON (Some1 Value) where
  mimeRender p (Some1 value) = mimeRender p value

data DependentValue

parseValue :: String -> Some1 Value
parseValue text = case reads @Int text of
  [(v, "")] -> Some1 $ ValueInt v
  _ -> Some1 $ ValueString text

valueDict :: Value a -> Dict (Typeable a, FromHttpApiData (Value a), FromHttpApiData (Operation a))
valueDict (ValueString _) = Dict
valueDict (ValueInt _) = Dict

type Result x = Capture' '[Required] "operation" (Operation x) :> Get '[JSON] (Value x)

data DependentServer
  = DependentServer
      ( forall m x.
        (Monad m) =>
        ServerT (Capture' '[Required] "value" (Value x) :> Capture' '[Required] "operation" (Operation x) :> Get '[JSON] (Value x)) m
      )

instance (ServerContext ctx) => HasServer DependentValue ctx where
  type ServerT DependentValue m = DependentServer

  hoistServerWithContext _ _ _ x = x

  route :: forall env. Proxy DependentValue -> Context ctx -> Delayed env (Server DependentValue) -> Router env
  route _ ctx delayedHandler = RawRouter rt
   where
    notFoundHandler :: NotFoundErrorFormatter
    notFoundHandler = notFoundErrorFormatter $ getContextEntry $ contextWithError ctx
    rt :: env -> Request -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived
    rt env req onResponse = case pathInfo req of
      (p : prest) -> do
        let
          req' = req{pathInfo = prest}
          inner :: ResourceT IO ResponseReceived
          inner = do
            result <- runDelayedIO (getResult delayedHandler env) req'
            case result of
              Route g -> do
                -- handlerRes <- undefined
                handlerRes <- liftIO $ runHandler $ runDependent g
                case handlerRes of
                  Right received -> return received
                  Left e -> liftIO $ onResponse $ Fail e
              Fail e -> liftIO $ onResponse $ Fail e
              FailFatal e -> liftIO $ onResponse $ FailFatal e
          -- Fail e -> liftIO $ onResponse $ Fail e

          runDependent :: DependentServer -> Handler ResponseReceived
          runDependent (DependentServer g) =
            let
              -- val = ValueString $ T.unpack p
              routed :: forall b. (FromHttpApiData (Operation b), Typeable b) => Value b -> Delayed env DependentServer -> Router env
              routed value (Delayed{..}) =
                route
                  (Proxy @(Result b))
                  EmptyContext
                  ( Delayed
                      { serverD = \cap params headers auth body req' -> Route (g value)
                      , ..
                      }
                  )
             in
              -- x value = runRouterEnv (notFoundErrorFormatter defaultErrorFormatters) (routed value delayedHandler) env req' onResponse
              do
                case parseValue $ T.unpack p of
                  (Some1 value) ->
                    useDict (valueDict value)
                      $
                      -- let httpDict value
                      liftIO
                      $ runRouterEnv notFoundHandler (routed value delayedHandler) env req' onResponse
         in
          -- liftIO x

          runResourceT inner

contextWithError :: Context ctx -> Context (ctx .++ DefaultErrorFormatters)
contextWithError = (.++ (defaultErrorFormatters :. EmptyContext))

getResult :: Delayed env a -> env -> DelayedIO a
getResult Delayed{..} env = do
  cap <- capturesD env
  methodD
  a <- authD
  content <- contentD
  p <- paramsD
  h <- headersD
  b <- bodyD content
  req <- ask
  liftRouteResult $ serverD cap p h a b req
