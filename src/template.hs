#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Module
--
-- Note: define a module to generate Haddock documentation per note
module NoteTemplate where

-- * main
--
-- $info
-- Infos

main :: IO ()
main = do
  -- Stuff
  return ()
