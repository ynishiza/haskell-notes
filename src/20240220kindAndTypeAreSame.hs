#!/usr/bin/env stack
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
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
module NoteKindAndTypeAreSame where

import Data.Kind (Type)
import GHC.Types (Constraint)

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  return ()

type HasSing :: forall {k}. k -> Constraint
class HasSing (a :: k) where -- k as kind
  type Sing a :: Type
  sing :: Sing a
  demote :: Sing a -> k -- k as Type

data SBool (a :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

instance HasSing ('True :: Bool) where -- Bool as kind
  type Sing 'True = SBool 'True
  sing = STrue
  demote _ = True -- Bool as Type

instance HasSing 'False where
  type Sing 'False = SBool 'False
  sing = SFalse
  demote _ = False

withSing :: forall r a. (HasSing a) => (Sing a -> r) -> r
withSing f = f (sing @a)
