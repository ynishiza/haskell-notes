#!/usr/bin/env stack
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Dependent types in Haskell:

  https://serokell.io/blog/ghc-dependent-types-in-haskell-2

New extensions

  RequiredTypeArguments: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/required_type_arguments.html#extension-RequiredTypeArguments

  TypeAbstractions:     https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_abstractions.html#extension-TypeAbstractions
-}
module NoteTemplate (
  -- * main

  --
  main,
) where

{- $info
Infos
-}

main :: IO ()
main = do
  putStrLn $ dosomething Int 1
  putStrLn $ dosomething Bool True
  putStrLn $ dosomething String "a"
  putStrLn $ id2 "a"
  return ()

-- Extension: RequiredTypeArguments
dosomething :: forall x -> (Show x) => x -> String
dosomething _ x = show x

-- Extension: TypeAbstractions
id2 :: a -> a
id2 @t v = v :: t
