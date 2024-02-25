#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module NoteExceptionHierachy where

import Control.Exception
import Data.Data (cast)
import Test.Hspec

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  hspec spec
  return ()

data MyExceptionA = forall e. (Exception e) => MyExceptionA e

instance Show MyExceptionA where 
  show (MyExceptionA e) = "MyExceptionA " <> show e

-- | SomeException (MyExceptionA e)
instance Exception MyExceptionA

data MyExceptionB = MyExceptionB deriving (Eq, Show)

-- | SomeException (MyExceptionB e)
instance Exception MyExceptionB

data MySubExceptionA = MySubExceptionA deriving (Eq, Show)

-- | SomeException (MyExceptionA (MySubExceptionA e))
instance Exception MySubExceptionA where
  toException = toException . MyExceptionA
  fromException e = do
    MyExceptionA e' <- fromException e
    cast e'

instance Eq SomeException where e1 == e2 = show e1 == show e2
instance Eq MyExceptionA where e1 == e2 = show e1 == show e2

spec :: SpecWith ()
spec = describe "Exception heirarchy" $ do
  it "toException" $ do
    show (toException (MyExceptionA DivideByZero)) `shouldBe` "MyExceptionA divide by zero"
    show (toException MySubExceptionA) `shouldBe` "MyExceptionA MySubExceptionA"
    show (toException MyExceptionB) `shouldBe` "MyExceptionB"

  let
    isMyExceptionA (MyExceptionA _) = True
    isMyExceptionB MyExceptionB = True
    isDivideByZero DivideByZero = True
    isDivideByZero _ = False

  it "throw/catch" $ do
    -- catches any MySubExceptionA (MySubSubException)
    try @MySubExceptionA @Int (throwIO MySubExceptionA) >>= (`shouldBe` Left MySubExceptionA)
    try @MySubExceptionA @Int (throwIO $ MyExceptionA DivideByZero) `shouldThrow` isMyExceptionA
    try @MySubExceptionA @Int (throwIO MyExceptionB) `shouldThrow` isMyExceptionB
    try @MySubExceptionA @Int (throwIO DivideByZero) `shouldThrow` isDivideByZero

    -- catches any MyExceptionA
    try @MyExceptionA @Int (throwIO MySubExceptionA) >>= (`shouldBe` Left (MyExceptionA MySubExceptionA))
    try @MyExceptionA @Int (throwIO $ MyExceptionA DivideByZero) >>= (`shouldBe` Left (MyExceptionA DivideByZero))
    try @MyExceptionA @Int (throwIO MyExceptionB) `shouldThrow` isMyExceptionB
    try @MyExceptionA @Int (throwIO DivideByZero) `shouldThrow` isDivideByZero

    -- catches only MyExceptionB
    try @MyExceptionB @Int (throwIO MySubExceptionA) `shouldThrow` isMyExceptionA
    try @MyExceptionB @Int (throwIO $ MyExceptionA DivideByZero) `shouldThrow` isMyExceptionA
    try @MyExceptionB @Int (throwIO MyExceptionB) >>= (`shouldBe` Left MyExceptionB)
    try @MyExceptionB @Int (throwIO DivideByZero) `shouldThrow` isDivideByZero
    --
    try @SomeException @Int (throwIO MySubExceptionA) >>= (`shouldBe` Left (SomeException $ MyExceptionA MySubExceptionA))
    try @SomeException @Int (throwIO $ MyExceptionA DivideByZero) >>= (`shouldBe` Left (SomeException $ MyExceptionA DivideByZero))
    try @SomeException @Int (throwIO MyExceptionB) >>= (`shouldBe` Left (SomeException MyExceptionB))
    try @SomeException @Int (throwIO DivideByZero) >>= (`shouldBe` Left (SomeException DivideByZero))
