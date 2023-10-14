#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Function
import GHC.Exts (IsString (..))

main :: IO ()
main = do
  print $ eitherDecode @MyData "{}"
  print $ eitherDecode @MyData "{ \"a\": {} }"
  print $ eitherDecode @MyData "{ \"a\": { \"b\": {} } }"
  print $ eitherDecode @MyData "{ \"a\": { \"b\": { \"c\": true } } }"
  print $ eitherDecode @MyBadData "{}"
  print $ eitherDecode @MyBadData "{ \"a\": {} }"
  print $ eitherDecode @MyBadData "{ \"a\": { \"b\": {} } }"
  print $ eitherDecode @MyBadData "{ \"a\": { \"b\": { \"c\": true } } }"

newtype MyData = MyData Bool deriving stock (Show, Eq)

newtype MyBadData = MyBadData Bool deriving stock (Show, Eq)

instance FromJSON MyData where
  -- parseJSON v =
  --   withObject "base" (.: "a") v
  --     >>= (<?> Key "a")
  --       . ( withObject "b" (.: "b")
  --             >=> (<?> Key "b")
  --               . ( withObject "c" (.: "c")
  --                     >=> withBool "" (return . MyData)
  --                 )
  --         )

  parseJSON = withObject "base" $ \obj -> do
    (Object a) <- obj .: "a"
    do
      (Object b) <- a .: "b"
      do
        (Bool c) <- b .: "c"
        return $ MyData c
        <?> Key "b"
      <?> Key "a"

instance FromJSON MyBadData where
  parseJSON = withObject "base" $ \obj -> do
    (Object a) <- obj .: "a"
    (Object b) <- a .: "b"
    (Bool c) <- b .: "c"
    return $ MyBadData c

inKey :: String -> Parser a -> Parser a
inKey key p = p <?> Key (fromString key)
