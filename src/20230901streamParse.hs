#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE OverloadedStrings #-}

-- Note: define a module to generate Haddock documentation per note
module Note20230901streamParse where

import Data.Attoparsec.ByteString.Char8
-- import Data.Attoparsec.ByteString.Streaming as BS
import Data.ByteString qualified as B
import Data.Functor.Of
import Streaming.ByteString

a :: Parser Char
a = char 'a'

stream :: ByteStream m ()
stream = fromStrict "aaaaabbb"

main :: IO ()
main = do
  return ()
  -- (Left res, restStream) <- BS.parse (many' a) stream
  -- (rest :> _) <- toStrict restStream
  -- putStrLn $ "result:" <> res
  -- B.putStr $ "rest:" <> rest <> "\n"
