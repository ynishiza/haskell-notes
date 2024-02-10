#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module NoteTemplate where

import Data.Time
import GHC.Exts (IsString (..))

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- print (2024 (1 :: Int) (20 :: Int) :: Day)
  print (2024 1 20 :: Day)
  print ("ABC" True :: MyData Bool)
  -- Stuff
  return ()

instance (a ~ Int, b ~ Int) => Num (a -> b -> Day) where
  -- instance Num (Int -> Int -> Day) where     BAD
  fromInteger = fromGregorian
  (+) = undefined
  (*) = undefined
  abs = undefined
  negate = undefined
  signum = undefined

data MyData a = MyData String a
  deriving (Show, Eq)

instance IsString (a -> MyData a) where
  fromString = MyData
