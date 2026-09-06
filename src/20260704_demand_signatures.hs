#!/usr/bin/env stack
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

    $ stack  ghc -- -O2 src/20260704_demand_signatures.hs   -ddump-dmd-signatures -fforce-recomp | less

==================== Demand signatures ====================
:Main.main: <L>
Main.$trModule:
Main.f: <S!P(L)>
Main.f2: <1!P(L)><1!P(L)>
Main.main: <L>

-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note
Note: Use export list since LSP rename feature isn't supported without it.
-}
module Main where
import Data.Functor.Identity

f1 :: Int -> Int
f1 x = x + x

f2 :: Int -> Int -> Int
f2 x y = x + y

f3 :: Bool -> Int -> Int
f3 t x = if t then x else 0

f4 :: (Int -> Int) -> Int
f4 g = g 0 -- 1C(1,L)
f4 g = let x = g 0 in x + x -- 1C(1,L)
-- f4 g = g 0 + g 0 -- SC(1,L)
-- f4 g = g 0 + g 1 -- SC(S,L)
--

f5 :: (Int -> Int -> Int) -> Int
f5 g = g 0 1 -- 1C(1,C(1,L))
-- f5 g = g 1 0 + g 2 0
-- f5 g = g 1 2 + g 3 4
-- f5 g = g 0 1 + g 0 2
-- f5 g = g 0 1 + g 0 1  -- SC(S,C(1,L))
-- f5 g = let g' = g 0 in g' 1 + g' 2  -- 1C(1,SC(S,L))

f6 :: (Int -> Int -> Int) -> Int -> Int -- 1(
f6 g = g 0

h0 :: Identity Int -> Int
h0 v@(Identity x) = case v of (Identity y) -> x + y
-- h0 (Identity x) = x

h00 :: m a -> m a
h00 x = x

h1 :: Maybe Int -> Int
h1 (Just x) = x  -- 1L
h1 Nothing = 0

h2 :: (Int, Double) -> Double
h2 (a, b) = b + fromIntegral a -- 1P(1L, 1L)

h3 :: a -> a
h3 a = a

h30 :: (Show a) => a -> String
h30 !a = show a <> show a

h4 :: (a, b) -> [a]
h4 (a, b) = [a, a] -- 1P(L, A)

h5 :: (a, a, a) -> [a]
h5 (a, b, c) = [a, a, b, c] -- 1P(L, L, L)

h6 :: a -> [a]  -- <L>
h6 a = [a, a, a] 

frst :: (a, b) -> a
frst (x, _) = x

h7 :: Maybe Int -> Maybe Int
h7 x = x

h8 :: Maybe Int -> Maybe Int
h8 (Just x) = Just x
h8 Nothing = Nothing

mb :: b -> (a -> b) -> Maybe a -> b  -- <ML><MC(1,L)><1L>
mb _ f (Just a) = f a
mb b _ Nothing = b

id2 :: x -> x
id2 x = x

sq :: a -> b -> b
sq x y = seq x y

er1 :: Show a => a -> b -> c
er1 x _ = error $ show x

er2 :: a -> b -> c
er2 x _ = undefined

er3 :: String -> b -> c
er3 x _ = error x

main :: IO ()
main = do
  putStrLn "Hello"
