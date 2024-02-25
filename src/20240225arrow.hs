#!/usr/bin/env stack
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module NoteArrow where

import Control.Arrow hiding (Kleisli (..))
import Control.Category
import Control.Monad
import Prelude hiding ((.))
import Prelude qualified

-- * main

--

{- $info
Infos
-}
dot :: (b -> c) -> (a -> b) -> a -> c
dot = (Prelude..)

main :: IO ()
main = do
  return  ()

test :: IO ()
test = do
  runKleisli programArrow ()
  programMonad ()
  return ()

newtype MyArrow a b c = MyArrow (a b c)

instance (Arrow a) => Functor (MyArrow a b) where
  fmap f (MyArrow k) = MyArrow $ arr f . k

instance (Arrow a) => Applicative (MyArrow a b) where
  pure x = MyArrow $ arr (const x)
  (<*>) :: MyArrow a b (c -> d) -> MyArrow a b c -> MyArrow a b d
  MyArrow j <*> MyArrow k = MyArrow $ arr (\(x, f) -> f x) . (k &&& j)

-- * Kleisli

--
newtype Kleisli m a b = Kleisli (a -> m b)

runKleisli :: Kleisli m a b -> a -> m b
runKleisli (Kleisli k) = k

instance (Monad m) => Category (Kleisli m) where
  id = Kleisli return
  Kleisli l . Kleisli k = Kleisli $ k >=> l

instance (Monad m) => Arrow (Kleisli m) where
  arr f = Kleisli $ dot return f
  first (Kleisli k) = Kleisli $ \(x, y) -> (,y) <$> k x

instance (Monad m) => ArrowChoice (Kleisli m) where
  left (Kleisli k) = Kleisli $ either ((Left <$>) . k) (return . Right)

instance (Monad m) => ArrowApply (Kleisli m) where
  app = Kleisli $ \(Kleisli k, x) -> k x

-- * Kleisli vs monad

programMonad :: () -> IO ()
programMonad =
  const (putStrLn "Enter input:")
    >=> const getLine
    >=> (putStrLn . ("You typed:" <>))

programArrow :: Kleisli IO () ()
programArrow =
  Kleisli (const $ putStrLn "Enter input:")
    >>> Kleisli (const getLine)
    >>> Kleisli (putStrLn . ("You typed:" <>))
