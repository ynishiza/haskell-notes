#!/usr/bin/env stack
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module NoteTemplate where

import Data.Kind (Constraint, Type)

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  putStrLn $ "zero:" <> show (toNatWithTypeClass @Zero)
  putStrLn $ "one:" <> show (toNatWithTypeClass @One)
  putStrLn $ "two + three:" <> show (addNatWithTypeClass @Two @Three)
  -- Stuff
  putStrLn $ "zero:" <> show (toNatWithSingleton $ singNat @Zero)
  putStrLn $ "three:" <> show (toNatWithSingleton $ singNat @Three)
  return ()

type Some :: k -> Type
data family Some a

data Nat = Z | S Nat

type Zero = 'Z
type One = 'S Zero
type Two = 'S One
type Three = 'S Two

-- * Pattern match with type class
type ToNat :: Nat -> Constraint
class ToNat n where
  toNatWithTypeClass :: Int

instance ToNat 'Z where toNatWithTypeClass = 0
instance (ToNat n) => ToNat ('S n) where toNatWithTypeClass = 1 + (toNatWithTypeClass @n)

type AddNat :: Nat -> Nat -> Constraint
class AddNat n b where
  addNatWithTypeClass :: Int

instance AddNat 'Z 'Z where addNatWithTypeClass = 0
instance (AddNat n 'Z) => AddNat ('S n) 'Z where addNatWithTypeClass = 1 + addNatWithTypeClass @n @'Z
instance (AddNat n b) => AddNat n ('S b) where addNatWithTypeClass = 1 + addNatWithTypeClass @n @b

-- ====================  Singleton ====================

-- * Pattern match with singleton
type SNat :: Nat -> Type
data SNat n where
  SS :: SNat n -> SNat ('S n)
  SZ :: SNat 'Z

data instance Some Nat = forall n. (SingNat n) => SomeNat (SNat n)

class SingNat n where
  singNat :: SNat n

instance SingNat 'Z where singNat = SZ
instance (SingNat n) => SingNat ('S n) where singNat = SS singNat

toNatWithSingleton :: SNat n -> Int
toNatWithSingleton SZ = 0
toNatWithSingleton (SS n) = 1 + toNatWithSingleton n

addNatWithSingleton :: SNat n -> SNat b -> Int
addNatWithSingleton SZ SZ = 0
addNatWithSingleton (SS n) SZ = 1 + addNatWithSingleton n SZ
addNatWithSingleton n (SS m) = 1 + addNatWithSingleton n m

toSNat :: Nat -> Some Nat
toSNat Z = SomeNat SZ
toSNat (S n) = case toSNat n of SomeNat b -> SomeNat (SS b)
