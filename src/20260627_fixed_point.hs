#!/usr/bin/env stack
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Testing fixed points

https://hackage.haskell.org/package/base/docs/Control-Monad-Fix.html#t:MonadFix
-}
module Strict (
  -- * main

  --
  main,
) where

import Control.Monad.Fix (MonadFix (mfix), fix)

repeat0 :: [Int]
repeat0 = fix (0 :)

-- badEnum :: [Int]
-- badEnum = fix (\v -> (head v + 1):v)

enumFrom' :: Int -> [Int]
enumFrom' = fix (\f -> (\n -> n : (f (n + 1)))) -- f :: Int -> [Int]

repeat12 :: [Int]
repeat12 = fix (\v -> (1 : 2 : v))

-- BAD: bottom
-- repeat12Bad :: [Int]
-- repeat12Bad = fix (\v -> (if head v == 1 then 2 else 1) : v)

-- BAD: bottom
-- repeat1Monad :: [Int]
-- repeat1Monad = mfix $ \v -> (1 : 2 : v + 1 : [])

factorial :: Int -> Int
factorial = fix $ \f -> (\n -> if n <= 1 then 1 else n * f (n - 1))

sum_ :: Int -> Int
sum_ x = foldl (+) x [1, 2, 3]

sum_' :: Int -> Int
sum_' x = foldl' (+) x [1, 2, 3]

double :: Int -> Int  -- not strict?
double x = x + x

double' :: Int -> Int -- strict in argument?
double' !x = x + x

double'' :: Int -> Int -- strict in argument + result?
double'' !x = let !y = x + x in y

main :: IO ()
main = do
  print $ take 10 $ repeat0
  -- print $ take 10 $ badEnum
  print $ take 10 $ enumFrom' 0
  print $ take 10 $ repeat12
  -- print $ take 10 $ repeat12Bad
  -- print $ take 10 $ repeat1Monad
  print $ factorial 2
  print $ factorial 3

  -- [(1,1),(2,2)]
  --
  -- since
  --
  --   (snd x, y) = x   =>   (y, y)
  --
  print $ mfix $ \x -> do
    w <- [1, 2]
    return (snd x, w)
  return ()
