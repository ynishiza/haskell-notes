#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import GHC.Exts (IsString)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status404)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Test.Hspec

main :: IO ()
main = hspec spec

testHost :: (IsString s) => s
testHost = "localhost"

testPort :: Port
testPort = 12345

type ShutdownHandler = TVar (IO ())

requestShutdown :: (MonadIO m) => ShutdownHandler -> m ()
requestShutdown = liftIO . join . readTVarIO

newShutdownHandler :: (MonadIO m) => m ShutdownHandler
newShutdownHandler = liftIO $ newTVarIO (pure ())

-- ========================================  Server ========================================
--

startServer :: (MonadIO m) => ShutdownHandler -> m ()
startServer shutdown =
  liftIO
    $ runSettings
      (testServerSettings shutdown)
      (serve (Proxy @API) handler)

testServerSettings :: ShutdownHandler -> Settings
testServerSettings shutdown =
  defaultSettings
    & setPort testPort
    & setHost testHost
    & setInstallShutdownHandler (atomically . writeTVar shutdown)

type API =
  "api"
    :> ( "a"
          :> QueryParam' '[Required] "name" String
          :> Get '[JSON] String
          :<|> ReqBody '[JSON] Int :> Post '[JSON] Int
       )

handler :: Server API
handler =
  (\name -> return $ "GET:" <> name)
    :<|> \num -> return (1 + num)

-- ========================================  Testing ========================================

oneSecond :: Int
oneSecond = 1000 * 1000

getCall :: String -> ClientM String
postCall :: Int -> ClientM Int
getCall :<|> postCall = client (Proxy @API)

type NonExistentEndpoint = "api" :> "nonexistent" :> Get '[JSON] ()

getNonExistent :: ClientM ()
getNonExistent = client (Proxy @NonExistentEndpoint)

testUrl :: BaseUrl
testUrl = BaseUrl Http testHost testPort ""

spec :: Spec
spec =
  ( \runTest -> do
      shutdownHandler <- newShutdownHandler
      manager <- newManager defaultManagerSettings
      let clientEnv = mkClientEnv manager testUrl

      _ <- forkIO $ do
        putStrLn $ "Starting server " <> testHost <> ":" <> show testPort
        startServer shutdownHandler

      threadDelay oneSecond
      runTest clientEnv
      requestShutdown shutdownHandler
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith ClientEnv
baseSpec = describe "API test" $ do
  let expectSuccess :: (HasCallStack) => ClientM a -> ClientEnv -> (a -> Expectation) -> Expectation
      expectSuccess call env onSuccess = do
        result <- runClientM call env
        case result of
          Right value -> onSuccess value
          Left e -> expectationFailure $ "Unexepcted error:" <> show e
      expectFailure :: (HasCallStack, Show a) => ClientM a -> ClientEnv -> (ClientError -> Expectation) -> Expectation
      expectFailure call env onFailure = do
        result <- runClientM call env
        case result of
          Right value -> expectationFailure $ "" <> show value
          Left f -> onFailure f

  it "GET /api/a/?name=<value>" $ \env -> do
    expectSuccess (getCall "abc") env (`shouldBe` "GET:abc")

  it "POST /api/a" $ \env -> do
    expectSuccess (postCall 1) env (`shouldBe` 2)

  it "[ERROR] non-existent endpoint" $ \env -> do
    expectFailure getNonExistent env $ \case
      (FailureResponse _ res) -> responseStatusCode res `shouldBe` status404
      e -> expectationFailure $ "Unexpected error:" <> show e
