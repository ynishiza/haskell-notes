#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- Note: define a module to generate Haddock documentation per note
module NoteTemplate where

main :: IO ()
main = putStrLn "Hello"
