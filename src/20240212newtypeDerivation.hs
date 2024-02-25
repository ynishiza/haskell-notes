#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module NoteNewtypeDerivation where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Writer
import GHC.Types (Type)
import Control.Monad.Trans.Control

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  return ()

class Monad m => SomeMonadClass (w :: Type) (m :: Type -> Type) where
  doSomething :: m w -> String
instance Monad m => SomeMonadClass w (ReaderT r m) where doSomething _ = ""

newtype MyAppB a = MyAppB (ReaderT String IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader String
    , MonadBase IO
    , MonadBaseControl IO
    , MonadIO 
    , SomeMonadClass b
    )

newtype MyApp r m a = MyApp (ReaderT r m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadWriter w
    , MonadBase (b :: Type -> Type)
    , MonadBaseControl (b :: Type -> Type)
    )

newtype MyAppStandalone r m a = MyAppStandalone (ReaderT r m a)
deriving newtype instance (Functor m) => Functor (MyAppStandalone r m)
deriving newtype instance (Applicative m) => Applicative (MyAppStandalone r m)
deriving newtype instance (Monad m) => Monad (MyAppStandalone r m)
deriving newtype instance (Monad m) => MonadReader r (MyAppStandalone r m)
deriving newtype instance (MonadWriter w m) => MonadWriter w (MyAppStandalone r m)
deriving newtype instance (MonadBase b m) => MonadBase b (MyAppStandalone r m)


