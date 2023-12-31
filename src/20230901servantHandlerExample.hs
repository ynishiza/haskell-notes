#!/usr/bin/env stack
-- Run with
--  stack exec -- src/scratch/<name>.hs
--  stack ghci -- src/scratch/<name>.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- Note: define a module to generate Haddock documentation per note
module Note20230901servantHandlerExample where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger
import Control.Monad.State
import Data.Function ((&))
import Servant

newtype MyHandler a = MyHandler (LoggingT (StateT String Handler) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadLogger
    , MonadError ServerError
    )

type MyAPI = Get '[JSON] String :<|> "number" :> Get '[JSON] Int

myAPI :: Proxy MyAPI
myAPI = Proxy

-- step: define with transforms
myAPIServer :: ServerT MyAPI MyHandler -- Server with MonadLogger
myAPIServer =
  ( do
      logDebugN "root called"
      pure "OK"
  )
    :<|> ( do
            logDebugN "number called"
            pure 1
         )

-- step: convert transforms to Server MyAPI
mainServer :: Server MyAPI -- Server MyAPI = ServerT MyAPI Handler
mainServer = hoistServer myAPI f myAPIServer
 where
  f :: MyHandler a -> Handler a -- converter
  f (MyHandler h) =
    runStdoutLoggingT h
      & flip evalStateT ""

main :: IO ()
main = pure ()
