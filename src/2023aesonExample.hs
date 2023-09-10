-- Run with 
--  stack exec -- src/scratch/<name>.hs
--  stack ghci -- src/scratch/<name>.hs
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics

data MyData1 where
  MyData1 ::
    { someInt :: Int,
      someString :: String
    } ->
    MyData1
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data MyData2 where
  MyData2 :: {someField :: Int} -> MyData2
  deriving stock (Show, Eq, Generic)

instance FromJSON MyData2 where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

instance ToJSON MyData2 where
  toEncoding =
    genericToEncoding $
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

data Alpha = A | B | C

instance FromJSON Alpha where
  parseJSON (String s)
    | s' == "a" = pure A
    | s' == "b" = pure B
    | s' == "c" = pure C
    | otherwise = parseFail "ERROR"
    where
      s' = T.unpack s
  parseJSON _ = parseFail "ERROR"

test :: IO ()
test = do
  print $ decode' @MyData1 "{ \"someInt\": 1, \"someString\": \"ABC\" }"
  print $ eitherDecode' @MyData1 "{}"
  print $ encode $ MyData1 1 "ABC"

  print $ decode' @MyData2 "{ \"some_field\": 1 }"
  print $ encode $ MyData2 {someField = 1}
