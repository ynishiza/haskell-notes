-- Run with
--
--    stack ghci -- src/scratch/ServantAPI.hs src/scratch/servantExample.hs
--
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import ServantAPI
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Function ((&))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import Control.Exception (SomeException)
import Control.Monad.Catch (try)

-- Error handlers
errorFromatter :: ErrorFormatters
errorFromatter =
  defaultErrorFormatters
    { notFoundErrorFormatter = \req ->
        err404 {errBody = "404 Not found:" <> BS.fromStrict (rawPathInfo req)}
    }

-- Server settings
serverSettings :: Settings
serverSettings =
  defaultSettings
    & setPort 1234
    & setHost "localhost"

start :: IO ()
start =
  serveWithContext api (errorFromatter :. EmptyContext) (hoistServer api handleError mainHandler)
    & logStdoutDev
    & runSettings serverSettings

handleError :: Handler a -> Handler a
handleError h = do
  result <- try h
  case result of
    Left (e :: SomeException) -> throwError $ err400 { errBody = BS.pack ("Uncaught error: " <> show e)  }
    Right x -> return x

