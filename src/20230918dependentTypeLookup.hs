#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- Note: define a module to generate Haddock documentation per note
module Note20230918dependentTypeLookup where

import Data.List.Singletons (SList (..))
import Data.Singletons.TH
import Data.Singletons.Base.TH (SBool(..))
import Data.Kind (Type)

main :: IO ()
main = putStrLn "Hello"

data Nat = Z | S Nat deriving (Eq, Show)

genSingletons [''Nat]

-- data SBool (a :: Bool) where
--   STrue :: SBool 'True
--   SFalse :: SBool 'False

-- type instance Sing = SBool

type family Len (x :: [a]) where
  Len '[] = 'Z
  Len (a : as) = 'S (Len as)

-- data SList (x :: [a]) where
--   SNil :: SList '[]
--   SCons :: (Sing (x :: a)) -> SList (xs :: [a]) -> SList (x : xs)

-- toList :: SingKind k => [Demote k] -> SList ('[] :: [k])
-- toList [] = SNil
-- instance SingI (x :: [a])
-- type instance Sing = SList

v0 = SNil :: SList ('[] :: [Bool])

v1 = sing:: SList '[ 'True]

v2 = sing :: SList '[ 'False, 'True]

v3 = sing :: SList '[ 'False, 'False, 'True]

data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

sLookup :: SingKind k => SList (l :: [k]) -> Fin (Len l) -> Demote k
sLookup (SCons a _) FZ = fromSing a
sLookup (SCons _ l) (FS n) = sLookup l n
sLookup SNil f = case f of {}

