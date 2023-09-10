#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Streaming as BS
import Data.ByteString qualified as B
import Data.Functor.Of
import Streaming.ByteString

a :: Parser Char
a = char 'a'

stream :: ByteStream m ()
stream = fromStrict "aaaaabbb"

main :: IO ()
main = do
  (Left res, restStream) <- BS.parse (many' a) stream
  (rest :> _) <- toStrict restStream
  putStrLn $ "result:" <> res
  B.putStr $ "rest:" <> rest <> "\n"
