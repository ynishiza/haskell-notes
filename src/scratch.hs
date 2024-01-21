#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- Note: define a module to generate Haddock documentation per note
module Scratch where

import Control.Monad.Free
import Control.Monad.Trans.Writer

main :: IO ()
main = do
  iterM interpretIO c
  putStrLn $ execWriter $ iterM prettyPrint c

  iterM interpretIO $ applyTestAction (Type . show <$> [1 :: Int, 10 ..]) c
  return ()

-- data StateEffect s a where
--  Get :: (s -> a) -> StateEffect s a
--  Put :: s -> a -> StateEffect s a
--  deriving stock (Functor)

-- data WriteEffect w a where
--  Tell :: w -> a -> WriteEffect w a
--  deriving stock (Functor)

-- data Effect s w a where
--  EfState :: StateEffect s a -> Effect s w a
--  EfWrite :: WriteEffect w a -> Effect s w a
--  deriving stock (Functor)

-- type EffectF s w = Free (Effect s w)

-- getF :: EffectF s w s
-- getF = liftF $ EfState $ Get id

-- putF :: s -> EffectF s w ()
-- putF s = liftF $ EfState $ Put s ()

-- tellF :: w -> EffectF s w ()
-- tellF w = liftF $ EfWrite $ Tell w ()

-- program :: EffectF Int String ()
-- program = do
--  x <- getF
--  putF x
--  tellF $ show x
--  return ()

---- data ReadEffect a where
----   ReadInt :: (Int -> a) -> ReadEffect a
----   ReadString :: (String -> a) -> ReadEffect a
----   deriving stock Functor

---- data WriteEffect a where
----   WriteInt :: Int -> a -> WriteEffect a
----   WriteString :: String -> a -> WriteEffect a
----   deriving stock Functor

---- data Effect a where
----   EffRead :: ReadEffect a -> Effect a
----   EffWrite :: WriteEffect a -> Effect a
----   A :: Effect Int -> (Int -> String) -> (String -> a) -> Effect a
----   deriving stock Functor

---- type EffF = Free Effect

---- readIntF :: EffF Int
---- readIntF = liftF $ EffRead $ ReadInt id

---- readStringF :: EffF String
---- readStringF = liftF $ EffRead $ ReadString id

---- writeIntF :: Int -> EffF ()
---- writeIntF x = liftF $ EffWrite $ WriteInt x ()

---- writeStringF :: String -> EffF ()
---- writeStringF x = liftF $ EffWrite $ WriteString x ()

---- program :: EffF ()
---- program = do
----   n <- readIntF
----   writeIntF n
----   s <- readStringF
----   writeStringF s
----   return ()
----

-- step: define simulator
data Effect a where
  PutStrLn :: String -> a -> Effect a
  GetLine :: (String -> a) -> Effect a
  deriving (Functor)

-- step: wrap in Free
type EffectF = Free Effect
putStrLnF :: String -> EffectF ()
putStrLnF s = liftF $ PutStrLn s ()

getLineF :: EffectF String
getLineF = liftF $ GetLine id

-- step: compute with Free
c :: EffectF ()
c = do
  putStrLnF "Hello"
  putStrLnF "World"
  x <- getLineF
  y <- getLineF
  putStrLnF ("x=" <> x <> ", y=" <> y)

-- e.g. interpret Free action as IO
interpretIO :: Effect (IO a) -> IO a
interpretIO (GetLine k) = putStr "Enter value:" >> getLine >>= k
interpretIO (PutStrLn s a) = putStrLn s >> a

-- e.g. interpret Free action as String
foldToString :: forall a. EffectF a -> String
foldToString (Pure _) = ""
foldToString (Free (PutStrLn s a)) = s <> ":" <> foldToString a
foldToString (Free (GetLine next)) = "GetLine 100:" <> foldToString (next "100")

prettyPrint :: Effect (Writer String a) -> Writer String a
prettyPrint (PutStrLn s a) = tell ("PutStrln " <> s <> "\n") >> a
prettyPrint (GetLine next) = tell "GetLine\n" >> next ""

data TestAction where
  Type :: String -> TestAction

applyTestAction :: [TestAction] -> EffectF a -> EffectF a
applyTestAction [] e = e
applyTestAction _ (Pure a) = Pure a
applyTestAction ((Type s) : xs) (Free (GetLine k)) = applyTestAction xs (k s)
applyTestAction xs (Free (PutStrLn s next)) = Free $ PutStrLn s $ applyTestAction xs next
