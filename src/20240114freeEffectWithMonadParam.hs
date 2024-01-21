#!/usr/bin/env stack
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

  MTL style for free: http://h2.jaguarpaw.co.uk/posts/mtl-style-for-free/
  What does free buy us: https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- Note: define a module to generate Haddock documentation per note
module NoteFreeEffectWithMonadParam where

import Control.Monad
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Trans.State qualified as S
import Data.Function
import Data.Functor
import GHC.IORef

main :: IO ()
main = putStrLn "Hello"

-- Note: don't call in main since it reads from stdin
test :: IO ()
test = do
  ref <- newIORef "Hello"

  join $ iterM (join . asIOState @IO ref) comp
  x <- readIORef ref
  putStrLn $ "final state:" <> x

  (res, y) <-
    iterM (join . asStateT) (comp @(S.StateT String IO))
      & flip S.runStateT "Hello"
  res >> putStrLn ("final state:" <> y)
  return ()

data MyStateT s m next where
  Lift :: m next -> MyStateT s m next
  Get :: (s -> m next) -> MyStateT s m next
  Put :: s -> m next -> MyStateT s m next

instance (Monad m) => Functor (MyStateT s m) where
  fmap f (Lift next) = Lift (f <$> next)
  fmap f (Get k) = Get ((f <$>) . k)
  fmap f (Put s next) = Put s (f <$> next)

type StateF s m = F (MyStateT s m)

getF :: (Monad m) => StateF s m s
getF = liftF $ Get return

putF :: (Monad m) => s -> StateF s m ()
putF s = liftF $ Put s (return ())

modifyF :: (Monad m) => (s -> s) -> StateF s m ()
modifyF f = do
  s <- getF
  putF $ f s

getsF :: (Monad m) => (s -> a) -> StateF s m a
getsF f = getF <&> f

ioF :: (MonadIO m) => IO a -> StateF s m a
ioF = liftF . Lift . liftIO

asIOState :: (MonadIO m) => IORef s -> MyStateT s m a -> m a
asIOState _ (Lift next) = next
asIOState ref (Get k) = liftIO (readIORef ref) >>= k
asIOState ref (Put s next) = liftIO (writeIORef ref s) >> next

asStateT :: (Monad m) => MyStateT s (S.StateT s m) a -> S.StateT s m a
asStateT (Lift next) = next
asStateT (Get k) = S.get >>= k
asStateT (Put k next) = S.put k >> next

comp :: (MonadIO m) => StateF String m (IO ())
comp = do
  x <- getF
  ioF $ putStrLn $ "current state: " <> x

  ioF $ putStrLn "Enter input"
  ioF getLine >>= putF

  return $ return ()
