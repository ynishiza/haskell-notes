#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import Prelude hiding (lookup)

main :: IO ()
main = do
  print $ eitherDecode @MyGoodData "{}" -- Left "Error in $: key \"a\" not found"
  print $ eitherDecode @MyGoodData "{ \"a\": {} }" -- Left "Error in $.a: key \"b\" not found"
  print $ eitherDecode @MyGoodData "{ \"a\": { \"b\": {} } }" -- Left "Error in $.a.b: key \"c\" not found"
  print $ eitherDecode @MyGoodData "{ \"a\": { \"b\": { \"c\": true } } }"
  print $ eitherDecode @MyBadData "{}" -- Left "Error in $: key \"a\" not found"
  print $ eitherDecode @MyBadData "{ \"a\": {} }" -- Left "Error in $: key \"b\" not found"
  print $ eitherDecode @MyBadData "{ \"a\": { \"b\": {} } }" -- Left "Error in $: key \"c\" not found"
  print $ eitherDecode @MyBadData "{ \"a\": { \"b\": { \"c\": true } } }"

newtype MyGoodData = MyGoodData Bool deriving stock (Show, Eq)

newtype MyBadData = MyBadData Bool deriving stock (Show, Eq)

instance FromJSON MyBadData where
  parseJSON = withObject "base" $ \obj -> do
    (Object a) <- obj .: "a"
    (Object b) <- a .: "b"
    (Bool c) <- b .: "c"
    return $ MyBadData c

instance FromJSON MyGoodData where
  parseJSON = withObject "base" $ \obj -> do
    (Object a) <- obj .: "a"
    do
      (Object b) <- a .: "b"
      do
        (Bool c) <- b .: "c"
        return $ MyGoodData c
        <?> Key "b"
      <?> Key "a"
