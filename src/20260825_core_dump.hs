#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/20260825_dump.hs

    stack  ghc -- src/20260825_dump.hs -ddump-simpl -dsuppress-all -dno-suppress-type-signatures -fforce-recomp | less
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note
Note: Use export list since LSP rename feature isn't supported without it.
-}
module NoteTemplate (
  -- * main

  --
  main,
  evalStateTMonadLazy,
) where

import Control.Monad.Trans.State.Lazy qualified as L

{- $info
Infos
-}
evalStateTMonadStrict :: Monad m => L.StateT s m a -> s -> m a
evalStateTMonadStrict m s = do 
    (a, _) <- L.runStateT m s
    return a
{-# INLINE evalStateTMonadStrict #-}

evalStateTMonadLazy :: Monad m => L.StateT s m a -> s -> m a
evalStateTMonadLazy m s = do 
    ~(a, _) <- L.runStateT m s
    return a
{-# INLINE evalStateTMonadLazy #-}

evalStateFunctorStrict :: Functor m => L.StateT s m a -> s -> m a
evalStateFunctorStrict m s = (\(a, _) -> a) <$> L.runStateT m s
{-# INLINE evalStateFunctorStrict #-}

evalStateFunctorLazy :: Functor m => L.StateT s m a -> s -> m a
evalStateFunctorLazy m s = (\ ~(a, _) -> a) <$> L.runStateT m s
{-# INLINE evalStateFunctorLazy #-}

main :: IO ()
main = do
  -- evalStateTMonadStrict (pure @(L.StateT String IO) ()) "monad strict"
  --   >>= print
  -- evalStateTMonadLazy (pure @(L.StateT String IO) ()) "monad lazy"
  --   >>= print
  -- evalStateFunctorStrict (pure @(L.StateT String IO) ()) "functor strict"
  --   >>= print
  -- evalStateFunctorLazy (pure @(L.StateT String IO) ()) "functor lazy"
  --   >>= print
  -- Stuff
  return ()
