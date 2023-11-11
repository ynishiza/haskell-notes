#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}

module Note20230901megaparsec where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace (trace)
import Data.Void

type Parser = Parsec Void String

enableDebug :: Bool
enableDebug = True

trace_ :: String -> a -> a
trace_ = if enableDebug then trace else (\_ x -> x)

d :: Parser a -> Parser a
d _p = _p <* tr

tr :: Parser ()
tr = do
  state <- getParserState
  trace_ (show state) $ return ()

p :: Parser String
p = string "abc" <|> string "def"

main :: IO ()
main = do
  parseTest p "abc"
  parseTest p "def"
  parseTest p "dzzzzz"

  putStrLn "===="
  parseTest (d $ count 3 (d $ p >> space) >> single 'd' >> single 'e') "abc\nabc\nabcdef"
  parseTest (d $ count 3 (d $ p >> space) >> single 'd' >> single 'e') "abc\nabc\nzz"

  return ()
