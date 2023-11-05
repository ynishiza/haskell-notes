#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

  Source: https://www.well-typed.com/blog/2015/12/dependently-typed-servers/
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe hiding (Handler)
import Control.Monad
import Data.Char (toLower, toUpper)
import Data.Data
import Data.Function
import Data.Kind
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client hiding (Response)
import Servant.Server.Internal.Delayed
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
  serve api apiHandler
    & runSettings settings

type API =
  "api"
    :> Summary "Sample"
    :> ( "hello"
          :> Get '[JSON] String
          :<|> DependentValueAPI
       )

api :: Proxy API
api = Proxy

apiHandler :: Server API
apiHandler = return "hello" :<|> dp
 where
  dp :: ServerT DependentValueAPI Handler
  dp = DependentValueHandler $ \v op -> return $ applyOpertion op v

data Dict (c :: Constraint) where
  Dict :: (c) => Dict c

withDict :: Dict c -> ((c) => a) -> a
withDict Dict x = x

-- ========================================  Test ========================================
--

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
  ( \runSpec -> do
      manager <- newManager defaultManagerSettings
      shutdownVar :: ShutdownHandler <- newTVarIO (pure ())
      let
        env = mkClientEnv manager testUrl
      void $ forkIO $ startServer $ serverSettings shutdownVar
      threadDelay oneSecond
      runSpec env
      shutdownServer shutdownVar
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith ClientEnv
baseSpec = describe "main" $ do
  let
    testSuccess :: (HasCallStack) => ClientEnv -> ClientM a -> (a -> Expectation) -> Expectation
    testSuccess env c onSuccess =
      runClientM c env >>= \case
        Left e -> expectationFailure $ "Received error:" <> show e
        Right v -> onSuccess v

    testFail :: (HasCallStack, Show a) => ClientEnv -> ClientM a -> (ClientError -> Expectation) -> Expectation
    testFail env c onSuccess =
      runClientM c env >>= \case
        Left e -> onSuccess e
        Right v -> expectationFailure $ "Expected error but received result " <> show v

  it "GET /api/<string>/upper" $ \env -> do
    testSuccess env (stringValue "abc" "upper") $ \x ->
      x `shouldBe` "ABC"

  it "GET /api/<string>/lower" $ \env -> do
    testSuccess env (stringValue "ABC" "lower") $ \x ->
      x `shouldBe` "abc"

  it "GET /api/<int>/double" $ \env -> do
    testSuccess env (intValue 123 "double") $ \x ->
      x `shouldBe` 246

  it "GET /api/<int>/negate" $ \env -> do
    testSuccess env (intValue 123 "negate") $ \x ->
      x `shouldBe` -123

  it "[error] GET /api/<int>/random" $ \env -> do
    testFail env (intValue 123 "random") $ \err -> do
      pending

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

parseValue :: String -> Either String (Some1 Value)
parseValue text = case reads @Int text of
  [(v, "")] -> Right $ Some1 $ ValueInt v
  _ -> Right $ Some1 $ ValueString text

-- ========================================  Dependent API ========================================

data DependentValueAPI

type ValueProperties a = (Typeable a, FromHttpApiData (Value a), FromHttpApiData (Operation a))
type ValueOperationAPI x = Capture' '[Required] "operation" (Operation x) :> Get '[JSON] (Value x)
type ValueCaptureAPI x = Capture' '[Required] "value" (Value x)

valueDict :: Value a -> Dict (ValueProperties a)
valueDict (ValueString _) = Dict
valueDict (ValueInt _) = Dict

runDependentValueHandler :: DependentValueHandler m -> Value a -> ServerT (ValueOperationAPI a) m
runDependentValueHandler (DependentValueHandler handler) value = withDict (valueDict value) $ handler value

data DependentValueHandler m
  = DependentValueHandler (forall x. (ValueProperties x) => ServerT (ValueCaptureAPI x :> ValueOperationAPI x) m)

instance (ServerContext ctx) => HasServer DependentValueAPI ctx where
  type ServerT DependentValueAPI m = DependentValueHandler m

  hoistServerWithContext _ ctx hoist (DependentValueHandler handler) = DependentValueHandler $ hoistInner hoist handler
   where
    hoistInner :: forall m n b. (ValueProperties b) => (forall x. m x -> n x) -> ServerT (ValueCaptureAPI b :> ValueOperationAPI b) m -> ServerT (ValueCaptureAPI b :> ValueOperationAPI b) n
    hoistInner = hoistServerWithContext (Proxy @(ValueCaptureAPI b :> ValueOperationAPI b)) ctx

  route :: forall env. Proxy DependentValueAPI -> Context ctx -> Delayed env (Server DependentValueAPI) -> Router env
  route _ ctx handler = RawRouter $ \env req onResponse -> case pathInfo req of
    [] -> onResponse $ Fail err500
    (p : prest) ->
      let path = T.unpack p
       in case parseValue path of
            Right v ->
              runRouterEnv
                onNotFoundError
                (routeToValue v handler)
                env
                req{pathInfo = prest}
                onResponse
            Left err -> onResponse $ Fail err500{errReasonPhrase = "Failed to parse path " <> path <> ". Error:" <> err}
   where
    onNotFoundError :: NotFoundErrorFormatter
    onNotFoundError = notFoundErrorFormatter $ getContextEntry $ contextWithDefaultErrorHandlers ctx

routeToValue :: forall env. Some1 Value -> Delayed env (DependentValueHandler Handler) -> Router env
routeToValue (Some1 (value :: Value b)) (Delayed{..}) =
  withDict (valueDict value)
    $ route
      (Proxy @(ValueOperationAPI b))
      EmptyContext
      ( Delayed
          { serverD = \cap params headers auth body req -> flip runDependentValueHandler value <$> serverD cap params headers auth body req
          , ..
          }
      )

contextWithDefaultErrorHandlers :: Context ctx -> Context (ctx .++ DefaultErrorFormatters)
contextWithDefaultErrorHandlers = (.++ (defaultErrorFormatters :. EmptyContext))
