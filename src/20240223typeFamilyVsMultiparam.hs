#!/usr/bin/env stack
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

--
-- Note: define a module to generate Haddock documentation per note

-- | Module
module NoteTypeFamilyVsMultiparamTypeClass where

import Data.Kind (Type)

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  print $ showForm1 @Int 1
  print $ showForm2 @Int 1
  print $ showForm3 @Int 1
  return ()

class (Show s) => ShowForm1 a s | a -> s where
  showForm1 :: a -> s

class (Show (Child2Type a)) => ShowForm2 a where
  type Child2Type a :: Type
  showForm2 :: a -> Child2Type a

type family Showable3 a :: Type

class (Show (Showable3 a)) => ShowForm3 a where
  showForm3 :: a -> Showable3 a

instance ShowForm1 Int String where
  showForm1 = show

instance ShowForm2 Int where
  type Child2Type Int = String
  showForm2 = show

type instance Showable3 Int = String

instance ShowForm3 Int where
  showForm3 = show
