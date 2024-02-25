#!/usr/bin/env stack
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module NoteLoggerContext where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List (intercalate)
import GHC.Types (Type)

-- * main

{- $info
Infos
-}

main :: IO ()
main = do
  runMyApp someApp defaultMyEnv
  return ()

-- * Base logger

{- $info
a
-}
type LogWriter = ByteString -> IO ()

class IsLoggerEnv env where
  _logWriter :: Lens' env LogWriter
  _logNamespaces :: Lens' env [String]

class LogContext m where
  localNamespace :: String -> m a -> m a

newtype ViaLogger (env :: Type) m a = ViaLogger (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env)

instance (MonadReader env m, IsLoggerEnv env) => LogContext (ViaLogger env m) where
  localNamespace context = local (over _logNamespaces (<> [context]))

instance (IsLoggerEnv env, MonadIO m, MonadReader env m) => MonadLogger (ViaLogger env m) where
  monadLoggerLog loc src level msg = do
    logger <- view _logWriter
    ctx <- view _logNamespaces
    let prefix = toLogStr $ "[" <> intercalate ":" ctx <> "] "
    liftIO $ logger $ fromLogStr $ defaultLogStr loc src level $ prefix <> toLogStr msg

-- * Base logger

{- $info
Info
-}
data MyEnv = MyEnv
  { myLogger :: LogWriter
  , myLogNamespace :: [String]
  }

defaultMyEnv :: MyEnv
defaultMyEnv = MyEnv B.putStrLn ["main"]

newtype MyApp a = MyApp (ReaderT MyEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader MyEnv, MonadIO)

deriving via (ViaLogger MyEnv (ReaderT MyEnv IO)) instance MonadLogger MyApp
deriving via (ViaLogger MyEnv (ReaderT MyEnv IO)) instance LogContext MyApp

instance IsLoggerEnv MyEnv where
  _logNamespaces f env =
    f (myLogNamespace env)
      <&> \x -> env{myLogNamespace = x}
  _logWriter f env =
    f (myLogger env)
      <&> \x -> env{myLogger = x}

runMyApp :: MyApp a -> MyEnv -> IO a
runMyApp (MyApp app) = runReaderT app

someApp :: MyApp ()
someApp = do
  $(logInfo) "Hello"
  localNamespace "UserService" $ do
    $(logDebug) "routing"
    localNamespace "ServiceA" $ do
      $(logInfo) "do A"
    localNamespace "ServiceB" $ do
      $(logInfo) "do B"
  localNamespace "ItemService" $ do
    $(logDebug) "routing"
    localNamespace "SearchService" $ do
      $(logError) "ERROR!"
