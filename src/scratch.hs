#!/usr/bin/env stack
{-# LANGUAGE DataKinds #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Type)

main :: IO ()
main = putStrLn "Hello"

data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

type Sing :: a -> Type
type family Sing where
  Sing = SBool

class SingKind k where
  s :: Sing (a :: k) -> k

instance SingKind Bool where
  s STrue = True
  s SFalse = False
