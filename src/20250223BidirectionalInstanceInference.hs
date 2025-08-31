#!/usr/bin/env stack
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Module
module NoteBidirectionalInstance (
  -- * main

  --
  main,
) where

import Data.Kind

{- $info

Reference: https://reasonablypolymorphic.com/blog/bidirectional-instance-contexts/index.html
-}

main :: IO ()
main = do
  print (value @(Int, Integer))
  -- Stuff
  return ()

type Dict :: Constraint -> Type
data Dict c = (c) => Dict

type MyClass :: Type -> Constraint
class MyClass a | a -> a where
  ev :: Dict (Evidence a)
  value :: a
  type family Evidence a :: Constraint
  type Evidence a = ()

instance MyClass Int where
  type instance Evidence Int = ()
  value = 0
  ev = Dict

instance MyClass Integer where
  type instance Evidence Integer = ()
  ev = Dict
  value = -1

instance forall a b. (MyClass a, MyClass b) => MyClass (a, b) where
  type instance Evidence (a, b) = (MyClass a, MyClass b)
  ev = Dict
  value = (value @a, value @b)

useEvidence :: forall a b. (MyClass a) => (Evidence a => b) -> b
-- useEvidence = (useEvi @a ev) 
useEvidence _ = undefined

useEvi :: Dict (Evidence a) -> (Evidence a => b) -> b
useEvi Dict f = f

forward :: Dict (MyClass a) -> Dict (MyClass b) -> Dict (MyClass (a, b))
forward Dict Dict = Dict

-- DOESN'T WORK?
backward :: Dict (MyClass (a, b)) -> (Dict (MyClass a), Dict (MyClass b))
backward Dict = undefined
