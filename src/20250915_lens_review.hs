#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Module
--
-- Note: define a module to generate Haddock documentation per note
-- Note: Use export list since LSP rename feature isn't supported without it.
module NoteTemplate
  ( -- * main

    --
    main,
    testReader,
  )
where

import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Exts (fromList, toList)
import Test.Hspec
import Utils

-- $info
-- Infos

mapVector :: Lens' (V.Vector a) [a]
mapVector f = ((fromList <$>) . f . toList)

arrayToList :: Traversal' (Value) [Value]
arrayToList = _Array . lens toList (const fromList)

lens2 :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens2 getter setter f v = (setter v) <$> f (getter v)

main :: IO ()
main = do
  testReader
  hspec test
  return ()

testReader :: IO ()
testReader = do
  putStrLn "==================== Reader ===================="
  let f :: Int -> String
      f = do
        y <- local (* 2) (show . (+ 1) <$> ask)
        x <- ask
        return $ "value:" <> show (x + read y)
      t :: (String, Int) = do
        tell "abc"
        tell "def"
        writer (10, "123")

  putStrLn $ f 10
  putStrLn $ show t

test :: Spec
test = describe "lens" $ do
  it "basic" $ do
    putStrLn "==================== Getting ===================="

    (1 :: Int) ^. id `shouldBe` 1
    (1 :: Int) ^. (id . to (* 2)) `shouldBe` 2
    (1 :: Int) ^. (id . lens (* 2) (const (`div` 2))) `shouldBe` 2
    (1 :: Int) ^. (id . lens2 (* 2) (const (`div` 2))) `shouldBe` 2
    (1 :: Int) ^. (id . lens show (const read)) `shouldBe` "1"

    (1 :: Int, True) ^. _1 `shouldBe` 1
    (1 :: Int, True) ^. _2 `shouldBe` True
    (1 :: Int, True) ^. (_1 . to (* 3)) `shouldBe` 3
    (1 :: Int, True) ^. (_2 . to not) `shouldBe` False

    -- get
    (1 :: Int, True) ^. (_1 . lens (* 10) (const id)) `shouldBe` 10
    (1 :: Int, True) ^. (_2 . lens not (const not)) `shouldBe` False
    (1 :: Int, True) ^. (_1 . lens (realToFrac @Int @Double) (const round)) `shouldBe` 1

  it "set" $ do
    -- set
    set (_1 . lens id (const (* 2))) 1 (1 :: Int, True) `shouldBe` (2 :: Int, True)
    ((3 :: Int, True) & (_1 . lens id (+)) .~ 10) `shouldBe` (13, True)
    set (_2 . lens id (const show)) True (1 :: Int, True) `shouldBe` (1, "True")
    set (_2 . lens id (const show)) False (1 :: Int, True) `shouldBe` (1, "False")

    preview (element 0) [1 :: Int, 2] `shouldBe` Just 1
    view (id) [1 :: Int, 2] `shouldBe` [1, 2]

  it "list" $ do
    let l = [(1, "a"), (2 :: Integer, "" :: String), (3, "z")]
    toListOf (traverse . _1) l `shouldBe` [1, 2, 3]
    toListOf (traverse . _2) l `shouldBe` ["a", "", "z"]
    toListOf (traverse . _1 . lens (* 2) (const id)) l `shouldBe` [2, 4, 6]

    l ^? (element 0) `shouldBe` Just (1, "a")
    l ^? (element 0 . _2) `shouldBe` Just "a"
    l ^.. (elements ((== 0) . (`mod` 2))) `shouldBe` [(1, "a"), (3, "z")]
    l ^.. (elements even) `shouldBe` [(1, "a"), (3, "z")]
    l ^.. (traverse . filtered (\(_, b) -> b < "m")) `shouldBe` [(1, "a"), (2, "")]
    l ^.. (traverse . _1 . lens show (const read)) `shouldBe` ["1", "2", "3"]

  it "aeson" $ do
    let lst = fromJust $ decodeStrict @Value "[1,2,3]"
        str = fromJust $ decodeStrict @Value "\"abc\""
        num = fromJust $ decodeStrict @Value "1"

    lst ^.. (_Array . mapVector . traverse) `shouldBe` [Number 1, Number 2, Number 3]
    lst ^.. (values . _Number . lens (* 2) (const id)) `shouldBe` [2, 4, 6]
    lst ^.. (arrayToList . traverse . _Number . lens (* 1000) (const id)) `shouldBe` [1000, 2000, 3000]

    str ^? (_String) `shouldBe` Just "abc"
    str ^?! (_String . lens ("Hello " <>) (const id)) `shouldBe` "Hello abc"
    str ^?! (_String . lens (T.unpack) (const T.pack)) `shouldBe` "abc"

    num ^?! (_Number . lens (round) (const (realToFrac @Integer))) `shouldBe` 1
