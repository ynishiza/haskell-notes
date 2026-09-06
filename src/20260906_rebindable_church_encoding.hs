#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-
  Run with
    stack exec -- src/20260906_rebindable_church_encoding.hs
    stack ghci -- src/20260906_rebindable_church_encoding.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE RankNTypes #-}

{- | Module

  Dependent if without dependent types: https://haskellforall.com/2026/09/dependent-if-expressions
-}
module NoteTemplate (
  -- * main

  --
  main,
) where

import Prelude (IO, Int, Monad (..), Num (fromInteger), String, print)

type Bool trueCase falseCase result = trueCase -> falseCase -> result

true :: Bool trueCase falseCase trueCase
true a _ = a

false :: Bool trueCase falseCase falseCase
false _ b = b

ifThenElse :: Bool trueCase falseCase result -> trueCase -> falseCase -> result
ifThenElse f = f

(&&) :: Bool z falseCase result -> Bool trueCase falseCase z -> trueCase -> falseCase -> result
f && g = \a b -> f (g a b) b

(||) :: Bool trueCase falseCase z -> Bool trueCase z result -> trueCase -> falseCase -> result
f || g = \a b -> g a (f a b)

example :: Bool Int String result -> result
example x = if x then 5 else "Hello"

main :: IO ()
main = do
  print (example true)
  print (example false)

  print (example (true && true)) -- 5
  print (example (false && true)) -- Hello
  print (example (true && false)) -- Hello
  print (example (false && false)) -- Hello

  print (example (true || false)) -- 5
  print (example (false || true)) -- 5
  print (example (false || false)) -- Hello
  print (example (true || true)) -- 5

-- type Pair x y = forall r. x -> y -> (x -> y -> r) -> r

-- first :: Pair x y -> x -> y -> x
-- first p x y = p x y (\x _ -> x)

-- second :: Pair x y -> x -> y -> y
-- second p x y = p x y (\_ y -> y)
