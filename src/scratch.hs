#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
-- Note: define a module to generate Haddock documentation per note
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Scratch where

import Control.Monad

main :: IO ()
main = do
  pure ()

countAndPrint :: String -> String -> IO ()
countAndPrint toMatch = readFile >=> (return . f) >=> print
 where
  f :: String -> Int
  f = length . filter (== toMatch) . words
