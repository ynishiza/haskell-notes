#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

For testing error handling of invalid errors i.e. bottoms.
In particular, using the `evaluate` function.

https://hackage-content.haskell.org/package/base/docs/Control-Exception.html#v:evaluate
-}
module NoteTemplate (
  -- * main

  --
  main,
) where

import Control.Exception (evaluate)
import Control.Exception.Base
import Data.Either
import GHC.Exception
import Test.ChasingBottoms
import Test.Hspec

{- $info
Infos
-}

-- BAD: can't be caught
x :: Int
x = error "OOPS"

y :: Int
y = x + 1

getY :: IO Int
getY = do
  result <- try @SomeException (evaluate x)
  case result of
    Left e -> do
      throw e
    Right v -> return $ v + 1

main :: IO ()
main = do
  try @SomeException getY >>= print
  testError
  hspec test

test :: Spec
test = describe "strictness" $ do
  it "basic" $ do
    True `shouldBe` True
    let e = error "OOPS"

    isBottom ((e, ())) `shouldBe` False
    isBottom (Just $ e) `shouldBe` False
    isBottom e `shouldBe` True

    isBottom (seq (Just e) ()) `shouldBe` False
    isBottom (seq e ()) `shouldBe` True

    (evaluate (e :: ())) `shouldThrow` (\(_ :: ErrorCall) -> True)
    -- (evaluate (Just (e :: ()))) `shouldThrow` (\(_ :: ErrorCall) -> True)   -- BAD
    (return $! (e :: ())) `shouldThrow` (\(_ :: ErrorCall) -> True)
    -- (return $ seq (e :: ()) ()) `shouldThrow` (\(_ :: ErrorCall) -> True)   -- BAD
    -- (return (e :: ())) `shouldThrow` () -- BAD

    (e :: IO ()) `shouldThrow` (\(_ :: ErrorCall) -> True)

    return (e :: ()) -- OK

testError :: IO ()
testError = do
  let checkResult :: String -> Either SomeException Int -> IO ()
      checkResult label (Left ex) = print $ label <> " ERROR" <> show ex
      checkResult label (Right v) = do
        try (evaluate v) >>= checkResult (label <> "Right")
      a = 1 `div` 0

  try (return a) >>= checkResult "return" -- Right
  try (return $ seq a a :: IO Int) >>= checkResult "return seq" -- Right
  try (return $! a) >>= checkResult "return $!" -- Left
  try (evaluate a) >>= checkResult "evaluate" -- Left
  Left e1 <- try @SomeException (return $! a)
  putStrLn $ "e1:" <> show e1
  Left e2 <- try @SomeException (evaluate a)
  putStrLn $ "e2:" <> show e2
  Left e3 <- try @SomeException (a `seq` return a)
  putStrLn $ "e3:" <> show e3
  Right v1 <- try @SomeException (return $ seq a a :: IO Int)
  -- print v1 -- crash
  Right v2 <- try @SomeException (return a :: IO Int)
  -- print v2 -- crash
  --
  return ()

rep :: Int -> a -> [a]
rep 0 _ = []
rep n v = let xs = rep (n - 1) v in xs `seq` (v : xs)
