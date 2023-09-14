#!/usr/bin/env stack
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}

import Data.Kind (Type)

main :: IO ()
main = putStrLn "Hello"

type (:~:) :: a -> b -> Type
data a :~: b where
  Refl :: a :~: a

sym :: a :~: b -> b :~: a
sym Refl = Refl

trans :: a :~: b -> b :~: c -> a :~: c
trans Refl Refl = Refl

inner :: f a :~: f b -> a :~: b
inner Refl = Refl

apply :: f :~: g -> a :~: b -> f a :~: f b
apply Refl Refl = Refl

cast :: a :~: b -> a -> b
cast Refl x = x

gcastWith :: a :~: b -> (a ~ b => r) -> r
gcastWith Refl f = f
