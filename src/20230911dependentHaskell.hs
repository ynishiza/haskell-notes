#!/usr/bin/env stack
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

import Data.Singletons.TH
import GHC.Natural
import Data.Kind (Type)

data Peano = Z | S Peano

genSingletons [''Peano]

type PeanoNatural :: Peano -> Type
data PeanoNatural n = PeanoNatural !(Sing n) Natural

pattern Zero :: (n ~ 'Z) => PeanoNatural n
pattern Zero = PeanoNatural SZ 0

data PN_Match n where
  PN_Match :: PeanoNatural n -> PN_Match n
  PNMismatch :: PN_Match n
x = PeanoNatural SZ 0

-- match :: PeanoNatural n -> PN_Match n

main :: IO ()
main = do
  pure ()
