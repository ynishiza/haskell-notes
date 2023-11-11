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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char (toLower, toUpper)
import Data.Data
import Data.Function
import Data.Kind
import Data.List (elemIndex)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status400)
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
          :<|> SomeCapture MyValue MyValueOperation
       )

api :: Proxy API
api = Proxy

apiHandler :: Server API
apiHandler = return "hello" :<|> dp
 where
  dp :: Server (SomeCapture MyValue MyValueOperation)
  dp = SomeCaptureHandler $ \v op -> return $ applyOperation op v

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
          :<|> Capture' '[Required] "value" Bool :> Capture' '[Required] "operation" String :> Get '[JSON] Bool
          :<|> Capture' '[Required] "value" Null :> Capture' '[Required] "operation" String :> Get '[JSON] Null
       )

stringValue :: String -> String -> ClientM String
intValue :: Int -> String -> ClientM Int
boolValue :: Bool -> String -> ClientM Bool
nothingValue :: Null -> String -> ClientM Null
stringValue :<|> intValue :<|> boolValue :<|> nothingValue = client (Proxy @ClientAPI)

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
    testSuccess env (stringValue "abc" "upper") (`shouldBe` "ABC")

  it "GET /api/<string>/lower" $ \env -> do
    testSuccess env (stringValue "ABC" "lower") (`shouldBe` "abc")

  it "GET /api/<string>/reverse" $ \env -> do
    testSuccess env (stringValue "ABC" "reverse") (`shouldBe` "CBA")

  it "GET /api/<int>/double" $ \env -> do
    testSuccess env (intValue 123 "double") (`shouldBe` 246)

  it "GET /api/<int>/negate" $ \env -> do
    testSuccess env (intValue 123 "negate") (`shouldBe` -123)

  it "GET /api/<bool>/not" $ \env -> do
    testSuccess env (boolValue True "not") (`shouldBe` False)

  it "GET /api/null/id" $ \env -> do
    testSuccess env (nothingValue Null "id") (`shouldBe` Null)

  it "GET /api/*/id" $ \env -> do
    testSuccess env (stringValue "abc" "id") (`shouldBe` "abc")
    testSuccess env (intValue 1 "id") (`shouldBe` 1)
    testSuccess env (boolValue True "id") (`shouldBe` True)

  it "[error] GET /api/<int>/random" $ \env -> do
    testFail env (intValue 123 "random") $ \case
      FailureResponse _ res -> responseStatusCode res `shouldBe` status400
      _ -> expectationFailure "unexpected error"

-- ========================================  MyValue ========================================
--

data Null = Null deriving stock (Show, Eq)

data MyValue a where
  ValueString :: String -> MyValue String
  ValueInt :: Int -> MyValue Int
  ValueBool :: Bool -> MyValue Bool
  ValueNull :: MyValue Null

data Some1 f = forall a. Some1 (f a)

instance Show (MyValue a) where
  show (ValueString v) = "StringValue " <> v
  show (ValueInt v) = "ValueInt " <> show v
  show (ValueBool v) = "ValueBool " <> show v
  show ValueNull = "ValueNull"

data Operation a where
  OpDouble :: Operation Int
  OpNegate :: Operation Int
  OpUpper :: Operation String
  OpLower :: Operation String
  OpReverse :: Operation String
  OpNot :: Operation Bool
  OpIdentity :: Operation a

instance Eq (Operation a) where
  x == y = operationName x == operationName y

class AllOperations a where
  allOperations :: [Operation a]

instance AllOperations Int where allOperations = [OpDouble, OpNegate, OpIdentity]
instance AllOperations String where allOperations = [OpUpper, OpLower, OpReverse, OpIdentity]
instance AllOperations Bool where allOperations = [OpNot, OpIdentity]
instance AllOperations Null where allOperations = [OpIdentity]

instance (AllOperations a) => Enum (Operation a) where
  toEnum i = allOperations !! (i `mod` length (allOperations @a))
  fromEnum v = fromJust $ elemIndex v allOperations

instance (AllOperations a) => Bounded (Operation a) where
  minBound = toEnum 0
  maxBound = toEnum (length (allOperations @a) - 1)

inverseMap :: forall e s. (Eq s, Enum e, Bounded e) => (e -> s) -> s -> Maybe e
inverseMap f input = foldr checkResult Nothing allValues
 where
  checkResult _ x@(Just _) = x
  checkResult value Nothing = if f value == input then Just value else Nothing
  allValues = enumFromTo (minBound @e) (maxBound @e)

applyOperation :: Operation a -> MyValue a -> MyValue a
applyOperation OpDouble (ValueInt v) = ValueInt $ 2 * v
applyOperation OpNegate (ValueInt v) = ValueInt $ negate v
applyOperation OpUpper (ValueString v) = ValueString $ toUpper <$> v
applyOperation OpLower (ValueString v) = ValueString $ toLower <$> v
applyOperation OpReverse (ValueString v) = ValueString $ reverse v
applyOperation OpNot (ValueBool v) = ValueBool $ not v
applyOperation OpIdentity v = v

operationName :: Operation a -> T.Text
operationName OpDouble = "double"
operationName OpNegate = "negate"
operationName OpUpper = "upper"
operationName OpLower = "lower"
operationName OpReverse = "reverse"
operationName OpNot = "not"
operationName OpIdentity = "id"

instance (AllOperations a) => FromHttpApiData (Operation a) where
  parseUrlPiece text =
    inverseMap operationName text
      & maybe (Left $ "Unsupported operation " <> text) Right

instance ToHttpApiData Null where
  toUrlPiece Null = "null"

instance Aeson.FromJSON Null where
  parseJSON Aeson.Null = return Null
  parseJSON _ = fail "Failed to parse Null"

instance Aeson.ToJSON Null where
  toJSON Null = error "Not used"
  toEncoding Null = Aeson.null_

instance MimeRender JSON (MyValue a) where
  mimeRender p (ValueInt v) = mimeRender p v
  mimeRender p (ValueString v) = mimeRender p v
  mimeRender p (ValueBool v) = mimeRender p v
  mimeRender _ ValueNull = "null"

instance MimeRender JSON (Some1 MyValue) where
  mimeRender p (Some1 value) = mimeRender p value

parseValue :: String -> Either String (Some1 MyValue)
parseValue text
  | (Just Null) <- Aeson.decode bytes = Right $ Some1 ValueNull
  | (Just v) <- (Aeson.decode @Bool) bytes = Right $ Some1 $ ValueBool v
  | (Just v) <- (Aeson.decode @Int) bytes = Right $ Some1 $ ValueInt v
  | otherwise = Right $ Some1 $ ValueString text
 where
  bytes = BL.fromStrict $ encodeUtf8 (T.pack text)

-- ========================================  Dependent API ========================================

contextWithDefaultErrorHandlers :: Context ctx -> Context (ctx .++ DefaultErrorFormatters)
contextWithDefaultErrorHandlers = (.++ (defaultErrorFormatters :. EmptyContext))

type family Apply1 (f :: Type) (x :: Type)

class ParseCapture value where
  parseCapture :: Text -> Either String value

instance ParseCapture (Some1 MyValue) where
  parseCapture = parseValue . T.unpack

class HasDependentServer (value :: Type -> Type) (api :: Type) where
  getDependentServer :: (ServerContext ctx) => value x -> Dict (HasServer (Apply1 api x) ctx)

data SomeCapture (value :: Type -> Type) (api :: Type)

data SomeCaptureHandler (value :: Type -> Type) api m = SomeCaptureHandler (forall x. value x -> ServerT (Apply1 api x) m)

instance
  ( ParseCapture (Some1 value)
  , HasDependentServer value api
  , ServerContext ctx
  ) =>
  HasServer (SomeCapture value api) ctx
  where
  type ServerT (SomeCapture value api) m = SomeCaptureHandler value api m

  route :: forall env. Proxy (SomeCapture value api) -> Context ctx -> Delayed env (SomeCaptureHandler value api Handler) -> Router env
  route _ ctx handler = RawRouter $ \env req onResponse -> case pathInfo req of
    [] -> onResponse $ Fail err500
    (p : prest) ->
      case parseCapture @(Some1 value) p of
        Right (Some1 value) ->
          runRouterEnv
            onNotFoundError
            (routeToHandler value handler)
            env
            req{pathInfo = prest}
            onResponse
        Left err -> onResponse $ Fail err500{errReasonPhrase = "Failed to parse path " <> T.unpack p <> ". Error:" <> err}
   where
    onNotFoundError :: NotFoundErrorFormatter
    onNotFoundError = notFoundErrorFormatter $ getContextEntry $ contextWithDefaultErrorHandlers ctx

    routeToHandler :: forall x. value x -> Delayed env (SomeCaptureHandler value api Handler) -> Router env
    routeToHandler value Delayed{..} =
      withDict ((getDependentServer @value @api @ctx) value)
        $ route
          (Proxy @(Apply1 api x))
          ctx
          Delayed
            { serverD = \cap params headers auth body req -> (\(SomeCaptureHandler f) -> f value) <$> serverD cap params headers auth body req
            , ..
            }

  hoistServerWithContext ::
    forall m n.
    Proxy (SomeCapture value api) ->
    Proxy ctx ->
    (forall x. m x -> n x) ->
    SomeCaptureHandler value api m ->
    SomeCaptureHandler value api n
  hoistServerWithContext _ ctx f (SomeCaptureHandler g) = SomeCaptureHandler $ \value -> hoistInner value (g value)
   where
    hoistInner :: forall x. value x -> ServerT (Apply1 api x) m -> ServerT (Apply1 api x) n
    hoistInner value = withDict ((getDependentServer @value @api @ctx) value) $ hoistServerWithContext (Proxy @(Apply1 api x)) ctx f

-- ========================================  Dependent API ========================================
--
type MyValueOperation :: Type
data MyValueOperation

type instance Apply1 MyValueOperation x = Capture' '[Required] "operation" (Operation x) :> Get '[JSON] (MyValue x)

instance HasDependentServer MyValue MyValueOperation where
  getDependentServer (ValueString _) = Dict
  getDependentServer (ValueInt _) = Dict
  getDependentServer (ValueBool _) = Dict
  getDependentServer ValueNull = Dict
