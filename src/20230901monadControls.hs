#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Note20230901monadControls where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import System.IO

main :: IO ()
main = putStrLn "Hello"

type MyLog = String

type MyEnv = Bool

type MyState = Int

type MyHandler a = (StateT MyState (WriterT MyLog (ReaderT MyEnv IO)) a)

myWithFile :: FilePath -> IOMode -> (Handle -> MyHandler a) -> MyHandler a -- writeFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a for MyHandler
myWithFile path mode = liftBaseOp (withFile path mode)

myWithFile2 :: FilePath -> IOMode -> (Handle -> MyHandler a) -> MyHandler a -- writeFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a for MyHandler
myWithFile2 path mode f = control $ \run -> withFile path mode (run . f)

liftBaseOp_2 :: MonadBaseControl b m => (b (StM m a) -> b (StM m c)) -> m a -> m c
liftBaseOp_2 f x = control $ \run -> f (run x)

liftBaseOp2 :: MonadBaseControl b m => ((a -> b (StM m c)) -> b (StM m d)) -> (a -> m c) -> m d
liftBaseOp2 f g = control $ \run -> f (run . g)

type X = StateT () (Writer String)
