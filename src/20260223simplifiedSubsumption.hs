#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- {-# LANGUAGE DeepSubsumption #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note
Note: Use export list since LSP rename feature isn't supported without it.
-}
module NoteTemplate (
  -- * main

  --
  main,
) where

{- $info
Infos
-}

abc :: Maybe (forall a. [a] -> [a]) -> Maybe ([Int], String)
abc Nothing = Nothing
abc (Just f) = Just (f [1, 2, 3], f "abc")

main :: IO ()
main = do
  let (Just (x, y)) = abc (Just reverse)

  putStrLn $ show x ++ y
  -- Stuff
  return ()

-- Simplified subsumption?
s :: ((forall a. a -> a) -> Bool) -> String
s _ = undefined

t :: (Int -> Int) -> Bool
t _ = undefined

u :: (forall a b. a -> b -> b) -> c
u = undefined

v :: forall a. a -> (forall b. b -> b)
v = undefined

-- x = s t          -- doesn't work
x :: String = s (\h -> t h)
-- y = u v          -- doesn't work
y= u (\a -> v a)

