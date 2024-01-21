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
module NoteTemplate where

import Control.Monad.Free
import Control.Monad.Trans.Writer

main :: IO ()
main = do
  iterM interpretIO $ applyTestAction (Type . show <$> [1 :: Int, 10 ..]) c
  return ()

-- Note: don't call in main since it reads from stdin
testIO :: IO ()
testIO = do
  iterM interpretIO c
  putStrLn $ execWriter $ iterM prettyPrint c

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
