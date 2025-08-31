#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Ref: https://www.haskell.org/arrows/biblio.html
Paper: "Generalizing Monads to Arrows"

Section 7 "Functors: new arrows from old"
-}
module NoteTemplate (
  main,

  -- * Maybe Functor
  MaybeFunctor (..),

  -- * State Functor
  StateFunctor (..),

  -- * CPS
  CPSFuctor (..),
) where

import Control.Arrow
import Control.Category
import Data.Either (fromLeft)
import Prelude hiding (id, (.))

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  return ()

-- ============================== Maybe ==============================

newtype MaybeFunctor a b c = MaybeFunctor {unMapbyeFunctor :: a b (Maybe c)}

liftMaybe :: (Arrow a) => a b c -> MaybeFunctor a b c
liftMaybe = MaybeFunctor . (>>^ Just)

instance (ArrowChoice a) => Category (MaybeFunctor a) where
  id = liftMaybe id
  MaybeFunctor cd . MaybeFunctor bc = MaybeFunctor $ maybeToEither bc >>> right cd >>> (nothing ||| id)

nothing :: (Arrow a) => a b (Maybe c)
nothing = constantArrow Nothing
maybeToEither :: (Arrow a) => a b (Maybe c) -> a b (Either () c)
maybeToEither = (>>^ maybe (Left ()) Right)

instance (ArrowChoice a) => Arrow (MaybeFunctor a) where
  arr = liftMaybe . arr
  first :: MaybeFunctor a b c -> MaybeFunctor a (b, d) (c, d)
  first (MaybeFunctor bc) = MaybeFunctor $ first bc >>^ (\(c, d) -> (,d) <$> c)

instance (ArrowChoice a) => ArrowZero (MaybeFunctor a) where
  zeroArrow = MaybeFunctor nothing

instance (ArrowChoice a) => ArrowChoice (MaybeFunctor a) where
  MaybeFunctor f ||| MaybeFunctor g = MaybeFunctor (f ||| g)
  left :: MaybeFunctor a b c -> MaybeFunctor a (Either b d) (Either c d)
  left (MaybeFunctor bc) =
    MaybeFunctor
      $ left bc
      >>^ ( \case
              Left (Just x) -> Just (Left x)
              _ -> Nothing
          )

instance (ArrowChoice a, ArrowApply a) => ArrowApply (MaybeFunctor a) where
  app :: MaybeFunctor a (MaybeFunctor a b c, b) c
  app = MaybeFunctor $ first unMapbyeFunctor ^>> app

-- ============================== StateFunctor ==============================

newtype StateFunctor s a b c = StateFunctor (a (b, s) (c, s))

liftState :: (Arrow a) => a b c -> StateFunctor s a b c
liftState = StateFunctor . first

instance (Arrow a) => Category (StateFunctor s a) where
  id = liftState id
  (.) :: StateFunctor s a c d -> StateFunctor s a b c -> StateFunctor s a b d
  StateFunctor cd . StateFunctor bc = StateFunctor (bc >>> cd)

instance (Arrow a) => Arrow (StateFunctor s a) where
  arr = liftState . arr
  first (StateFunctor f) = StateFunctor $ g ^>> first f >>^ g
   where
    g ((a, s), d) = ((a, d), s)

-- ============================== CPS ==============================
--

newtype CPSFuctor r a b c = CPSFuctor {unCPSFunctor :: a c r -> a b r}

liftCPS :: (Arrow a) => a b c -> CPSFuctor r a b c
liftCPS bc = CPSFuctor (bc >>>)

instance (Arrow a) => Category (CPSFuctor r a) where
  id = liftCPS id
  (.) :: CPSFuctor r a c d -> CPSFuctor r a b c -> CPSFuctor r a b d
  CPSFuctor fcd . CPSFuctor fbc = CPSFuctor $ \dr -> fbc (fcd dr)

constantArrow :: (Arrow a) => c -> a b c
constantArrow c = arr (const c)

instance (ArrowApply a) => Arrow (CPSFuctor r a) where
  arr = liftCPS . arr
  first :: forall b c d. CPSFuctor r a b c -> CPSFuctor r a (b, d) (c, d)
  first (CPSFuctor f) = CPSFuctor $ \k ->
    app
      <<< arr
        ( \(b, d) ->
            let cd = id &&& constantArrow d
             in (f (cd >>> k), b)
        )

instance (ArrowApply a, ArrowZero a) => ArrowZero (CPSFuctor r a) where
  zeroArrow = CPSFuctor (zeroArrow >>>)

instance (ArrowApply a) => ArrowChoice (CPSFuctor r a) where
  left :: forall b c d. CPSFuctor r a b c -> CPSFuctor r a (Either b d) (Either c d)
  left (CPSFuctor f) = CPSFuctor $ \k ->
    let x = f (Left ^>> k)
     in arr
          ( \case
              Left b -> (fromLeft b ^>> x, Left b)
              Right d -> (constantArrow (Right d) >>> k, Right d)
          )
          >>> app

instance (ArrowApply a) => ArrowApply (CPSFuctor r a) where
  app :: CPSFuctor r a (CPSFuctor r a b c, b) c
  app = CPSFuctor $ \k -> arr (\(CPSFuctor f, b) -> (f k, b)) >>> app
