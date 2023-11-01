#!/usr/bin/env stack
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

  Source: https://www.well-typed.com/blog/2015/12/dependently-typed-servers/
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char (toLower, toUpper)
import Data.Function
import Data.Text.IO qualified as T
import Data.Text qualified as T
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = do
  let serverInfo = layout api
  T.putStrLn serverInfo
  -- startServer

serverSettings :: Settings
serverSettings =
  defaultSettings
    & setPort 12345
    & setHost "localhost"

startServer :: IO ()
startServer =
  serve api handler
    & runSettings serverSettings

type API =
  "api"
    :> Summary "Sample"
    :> "hello"
    :> Get '[JSON] String

api :: Proxy API
api = Proxy

handler :: Server API
handler = return "hello"

-- ========================================  Value ========================================
--

data Value a where
  ValueString :: String -> Value String
  ValueInt :: Int -> Value Int

data Some1 f = forall a. Some1 (f a)

x = Some1 $ ValueString ""

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
  parseUrlPiece ((reads @Int) . T.unpack -> [(v,_)]) = Right $ ValueInt v

instance FromHttpApiData (Value String) where
  parseUrlPiece text = Right $ ValueString $ T.unpack text

-- instance MimeRender JSON (Value Int) where
--   mimeRender p (ValueInt v) = mimeRender p v

instance MimeRender JSON (Value a) where
  mimeRender p (ValueInt v) = mimeRender p v
  mimeRender p (ValueString v) = mimeRender p v

