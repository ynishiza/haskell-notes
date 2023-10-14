#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Aeson
import Data.Aeson.KeyMap
import Data.Aeson.Types
import Data.Text (Text)
import Prelude hiding (lookup)

main :: IO ()
main = do
  print $ eitherDecode @MyDataBad1 "{\"c1\": true }"
  print $ eitherDecode @MyDataBad1 "{\"c\": 1 }"     -- Left "Error in $: key \"c2\" not found"
  print $ eitherDecode @MyDataBad1 "{\"c1\": 1 }"    -- Left "Error in $: key \"c2\" not found"
  print $ eitherDecode @MyDataBad1 "{\"c2\": 1 }"    -- Left "Error in $.c2: parsing Text failed, expected String, but encountered Number"

  print $ eitherDecode @MyDataBad2 "{\"b1\": true }"
  print $ eitherDecode @MyDataBad2 "{\"c\": 1 }"      -- Left "Error in $: b1 | b2"
  print $ eitherDecode @MyDataBad2 "{\"b1\": 1 }"     -- Left "Error in $: b1 | b2"
  print $ eitherDecode @MyDataBad2 "{\"b2\": 1 }"     -- Left "Error in $: b1 | b2"

  print $ eitherDecode @MyDataGood "{\"a1\": true }"
  print $ eitherDecode @MyDataGood "{\"c\": 1 }"      -- Left "Error in $: a1 or a2"
  print $ eitherDecode @MyDataGood "{\"a1\": 1 }"     -- Left "Error in $.a1: parsing a1 failed, expected Boolean, but encountered Number"
  print $ eitherDecode @MyDataGood "{\"a2\": 1 }"     -- Left "Error in $.a2: parsing a2 failed, expected String, but encountered Number"

data MyDataBad1 = C1 Bool | C2 Text deriving (Eq, Show)

data MyDataBad2 = B1 Bool | B2 Text deriving (Eq, Show)

data MyDataGood = A1 Bool | A2 Text deriving (Eq, Show)

instance FromJSON MyDataBad1 where
  parseJSON = withObject "MyDataBad1" $ \obj -> do
    C1 <$> obj .: "c1" <|> C2 <$> obj .: "c2"

instance FromJSON MyDataBad2 where
  parseJSON = withObject "MyDataBad2" $ \obj -> do
    B1 <$> obj .: "b1" <|> B2 <$> obj .: "b2" <|> fail "b1 | b2"

instance FromJSON MyDataGood where
  parseJSON = withObject "A" $ \obj -> do
    case lookup "a1" obj of
      Just v -> withBool "a1" (return . A1) v <?> Key "a1"
      Nothing -> case lookup "a2" obj of
        Just v -> withText "a2" (return . A2) v <?> Key "a2"
        Nothing -> fail "a1 or a2"
