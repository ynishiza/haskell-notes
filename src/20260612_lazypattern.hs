#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Lazy pattern in BangPatterns: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html

Note: define a module to generate Haddock documentation per note
Note: Use export list since LSP rename feature isn't supported without it.
-}
module NoteTemplate (
  -- * main

  --
  main,
  test,
  listLazyTest2,
  listLazyTest,
  listStrictTest,
  scalarStrictTest,
  scalarLazyTest,
  f,
  f2,
  g,
  g2,
) where

test :: Int
test = 1

listLazyTest2 :: [Int] -> IO ()
listLazyTest2 v = do
  let ~(x : _) = v
  putStrLn $ show x

listLazyTest :: [Int] -> IO ()
listLazyTest v = do
  let ~(x : _) = v
  putStrLn "Hello"

listStrictTest :: [Int] -> IO ()
listStrictTest v = do
  -- let !(!x: _) = v
  let (x : _) = v
  seq x $ putStrLn "Hello"

scalarLazyTest :: Int -> IO ()
scalarLazyTest v = do
  let ~x = v
  putStrLn "Hello"

scalarStrictTest :: Int -> IO ()
scalarStrictTest v = do
  -- let !x = v
  let x = v
  seq x $ putStrLn "Hello"

f :: (a, b) -> Int -- f undefined = 0
f ~(x, _) = 0

f2 :: (a, b) -> Int -- f2 undefined = undefined
f2 (x, _) = 0

g :: Int -> Int -- g undefined = 0
g ~x = 0

g2 :: Int -> Int -- g2 undefined = 0
g2 x = 0

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  return ()
