#!/usr/bin/env stack
{--
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
--}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Control.Exception
import GHC.Stack

data MyException where
  MyException :: HasCallStack => String -> MyException

deriving instance Show MyException

deriving instance Eq MyException

deriving instance Exception MyException

fnThrow :: HasCallStack => IO ()
fnThrow = throw $ MyException "Oops"

fn0 :: IO ()
fn0 = putStrLn $ prettyCallStack callStack

fn1 :: HasCallStack => IO ()
fn1 = putStrLn $ prettyCallStack callStack

fn2 :: HasCallStack => IO ()
fn2 = fn1

fn3 :: HasCallStack => IO ()
fn3 = fn2

fn4 :: HasCallStack => IO ()
fn4 = fn3

fn3' :: IO ()
fn3' = fn2

fn4' :: HasCallStack => IO ()
fn4' = fn3'

g1 :: HasCallStack => ()
g1 = error "OOPS"

g2 :: HasCallStack => ()
g2 = g1

g3 :: ()
g3 = g2

g4 :: HasCallStack => ()
g4 = g3

handleError :: IO () -> IO ()
handleError action = catch action handler
  where
    handler (MyException s) = do
      putStrLn $ "ERROR: " <> show s <> "\n cs" <> prettyCallStack callStack

handleError2 :: IO () -> IO ()
handleError2 action = catch action handler
  where
    handler (SomeException s) = do
      putStrLn $ "ERROR: " <> show s <> "\n cs" <> prettyCallStack callStack

main :: HasCallStack => IO ()
main = do
  -- handleError fn0
  -- handleError fn4
  -- handleError fn4'
  handleError (evaluate g4)
  -- handleError2 fn4
