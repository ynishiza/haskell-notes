#!/usr/bin/env stack
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- Note: define a module to generate Haddock documentation per note
module Scratch where

import GHC.TypeLits
import Servant

data X

type API = "api" :> ReqBody  String -> Get '[JSON] String

handler :: Server API
handler = undefined

main :: IO ()
main = do
  serve (Proxy @API)
  -- let x = [1, 2 :: Int] + 1
  return ()





-- x = [1,2,3] + 4

instance
  ( TypeError
      ( 'Text "Bad number [" :<>: ShowType a :<>: 'Text "]"
          :$$: 'Text "ERROR" )
  ) =>
  Num [a]
