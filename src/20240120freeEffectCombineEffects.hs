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
module NoteFreeEffectCombineEffects where

import Control.Monad
import Control.Monad.Free.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.State qualified as S
import Data.Function
import GHC.IORef

main :: IO ()
main = putStrLn "Hello"

-- Note: don't call in main since it reads from stdin
test :: IO ()
test = do
  ref <- newIORef (0 :: Int)
  iterM (interpretIO ref) program1

  (_, s) <-
    iterM interpretStateT program1
      & flip S.runStateT (0 :: Int)
  putStrLn $ "Final state:" <> show s
  pure ()

-- ======================================================================

-- * Free

{- $info
Basic free
-}
data Free f a = Pure a | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free t) = Free $ (f <$>) <$> t

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  Pure a >>= k = k a
  Free t >>= k = Free $ (>>= k) <$> t

instance (Functor f) => MonadFree f (Free f) where
  wrap = Free

-- ======================================================================

-- * CPS Free

{- $info
CPS free
-}
data F f a = F (forall r. (a -> r) -> (f r -> r) -> r)

runFWith :: (a -> r) -> (f r -> r) -> F f a -> r
runFWith kp kf (F fld) = fld kp kf

instance (Functor f) => Functor (F f) where
  fmap f (F fld) = F $ \kp kf -> fld (kp . f) kf

instance (Functor f) => Applicative (F f) where
  pure a = F $ \kp _ -> kp a
  F fldf <*> F fldx = F $ \kp kf -> fldf (\f' -> fldx (kp . f') kf) kf

instance (Functor f) => Monad (F f) where
  (F fld) >>= k = F $ \kp kf -> fld (runFWith kp kf . k) kf

instance (Functor f) => MonadFree f (F f) where
  wrap x = F $ \kp kf -> kf (runFWith kp kf <$> x)

toF :: (Functor f) => Free f a -> F f a
toF (Pure a) = F $ \kp _ -> kp a
toF (Free f) = F $ \kp kf -> kf $ runFWith kp kf . toF <$> f

toFree :: F f a -> Free f a
toFree (F fld) = fld Pure Free

iterM :: (Monad m) => (f (m a) -> m a) -> F f a -> m a
iterM = runFWith pure

-- ======================================================================
--

-- * Commands

--

-- * Console Commands
data Console a where
  GetLine :: (String -> a) -> Console a
  PutStrLn :: String -> a -> Console a
  deriving stock (Functor)

interpretConsoleIO :: Console (IO a) -> IO a
interpretConsoleIO (GetLine k) = getLine >>= k
interpretConsoleIO (PutStrLn s next) = putStrLn s >> next

interpretConsoleStateT :: Console (StateT s IO a) -> StateT s IO a
interpretConsoleStateT (GetLine k) = liftIO getLine >>= k
interpretConsoleStateT (PutStrLn s next) = liftIO (putStrLn s) >> next

-- ** State Commands
data State s a where
  Get :: (s -> a) -> State s a
  Put :: s -> a -> State s a
  deriving stock (Functor)

interpretStateIO :: IORef s -> State s (IO a) -> IO a
interpretStateIO ref (Get k) = readIORef ref >>= k
interpretStateIO ref (Put s next) = writeIORef ref s >> next

interpretStateStateT :: (Monad m) => State s (StateT s m a) -> StateT s m a
interpretStateStateT (Get k) = S.get >>= k
interpretStateStateT (Put s next) = S.put s >> next

data Command s a where
  StateCommand :: State s a -> Command s a
  ConsoleCommand :: Console a -> Command s a
  deriving stock (Functor)

type ConsoleF = F Console

type StateF s = F (State s)

-- ** Command

type CommandF s = F (Command s)
getLineF :: CommandF s String
getLineF = liftF $ ConsoleCommand $ GetLine id

putStrLnF :: String -> CommandF s ()
putStrLnF s = liftF $ ConsoleCommand $ PutStrLn s ()

printF :: (Show a) => a -> CommandF s ()
printF = putStrLnF . show

getF :: CommandF s s
getF = liftF $ StateCommand $ Get id

putF :: s -> CommandF s ()
putF s = liftF $ StateCommand $ Put s ()

modifyF :: (s -> s) -> CommandF s ()
modifyF f = getF >>= putF . f

interpretIO :: IORef s -> Command s (IO a) -> IO a
interpretIO _ (ConsoleCommand c) = interpretConsoleIO c
interpretIO ref (StateCommand c) = interpretStateIO ref c

interpretStateT :: Command s (StateT s IO a) -> StateT s IO a
interpretStateT (ConsoleCommand c) = interpretConsoleStateT c
interpretStateT (StateCommand c) = interpretStateStateT c

program1 :: forall s. (Show s, Read s) => CommandF s ()
program1 = do
  putStrLnF "Hello"
  x <- getF
  putStrLnF $ "current state:" <> show x

  putStrLnF "Enter new state"
  n <- getLineF
  let x' = read @s n
  putStrLnF $ "current state:" <> show x'
  putF x'
  pure ()
