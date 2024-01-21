#!/usr/bin/env stack
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

  Free monads for EDSLs: https://serokell.io/blog/introduction-to-free-monads#free-monads-for-edsls
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Note: define a module to generate Haddock documentation per note
module NoteFreeEffectBasic where

import Control.Monad.Free
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.State qualified as S
import Control.Monad.Trans.Writer
import Data.Function
import GHC.IORef

main :: IO ()
main = do
  let (_, output) =
        iterM prettyPrint program1
          & runWriter
  putStrLn output

-- Note: don't call in main since it reads from stdin
testIO :: IO ()
testIO = do
  ref <- newIORef (0 :: Int)
  iterM (interpretIO ref) program1
  s <- readIORef ref
  putStrLn $ "interpretIO state:" <> show s

  (_, s') <-
    iterM interpretStateT program1
      & flip S.runStateT s
  putStrLn $ "interpretStateT state:" <> show s'

  let (_, output) =
        iterM prettyPrint program1
          & runWriter
  putStrLn output

  return ()

program1 :: StateF Int ()
program1 = do
  n <- getF
  putF (n + 1)
  modifyF (* 2)
  return ()

-- ======================================================================
--

-- ** State Commands
data State s a where
  Get :: (s -> a) -> State s a
  Put :: s -> a -> State s a
  deriving stock (Functor)

interpretIO :: IORef s -> State s (IO a) -> IO a
interpretIO ref (Get k) = readIORef ref >>= k
interpretIO ref (Put s next) = writeIORef ref s >> next

interpretStateT :: (Monad m) => State s (StateT s m a) -> StateT s m a
interpretStateT (Get k) = S.get >>= k
interpretStateT (Put s next) = S.put s >> next

prettyPrint :: State Int (Writer String a) -> Writer String a
prettyPrint (Get k) = tell ("GET" <> "\n") >> k 0
prettyPrint (Put _ next) = tell ("PUT" <> "\n") >> next

type StateF s = Free (State s)

getF :: StateF s s
getF = liftF $ Get id

putF :: s -> StateF s ()
putF s = liftF $ Put s ()

modifyF :: (s -> s) -> StateF s ()
modifyF f = getF >>= putF . f
