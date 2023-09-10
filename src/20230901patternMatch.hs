#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

main :: IO ()
main = do
  print $ isSumEven2 1 1
  print $ isSumEven2 1 2

isSumEven :: Int -> Int -> Bool
isSumEven x y = even (x + y)

isEven :: Int -> Bool
isEven (even -> True) = True
isEven (even -> False) = False

-- Not exhaustive?
isSumEven2 :: Int -> Int -> Bool
isSumEven2 x (isSumEven x -> True) = True
isSumEven2 x (isSumEven x -> False) = False

f :: Maybe (String -> Int -> Bool, String, Int) -> Bool
f (Just (g, s, g s -> True)) = True
f (Just (g, s, g s -> False)) = True
