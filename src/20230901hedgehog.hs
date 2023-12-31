#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
module Note20230901hedgehog where

import Control.Monad
import Data.Foldable
import GHC.Exts (fromString)
import Hedgehog
import Hedgehog.Gen as G
import Hedgehog.Internal.Seed
import Hedgehog.Range as R

main :: IO ()
main = do
  putStr "Hello"
  testSuccess <- checkSequential allTests
  unless testSuccess $ error "Test failed"

allTests :: Group
-- allTests = $$discover
allTests = $$(discoverPrefix "prop")

prop_linear :: Property
prop_linear = testCoverage $ int $ linear 1 100

prop_linearWithOrigin :: Property
prop_linearWithOrigin = testCoverage $ int (linearFrom 50 1 100)

prop_linearWithResize :: Property
prop_linearWithResize = testCoverage $ resize 30 $ int $ linear 1 100

prop_linearWithResizeOrigin :: Property
prop_linearWithResizeOrigin = testCoverage $ resize 30 $ int $ linearFrom 50 1 100

prop_a :: Property
prop_a = testCoverage $ int (R.constant 1 100)

prop_b :: Property
prop_b = testCoverage $ int (R.exponential 1 100)

prop_c :: Property
prop_c = property $ do
  x <- forAll $ int (R.constant 1 100)
  (x < 200) === True

testCoverage :: Gen Int -> Property
testCoverage gen = property $ forAll gen >>= checkCoverage

checkCoverage :: (MonadTest m) => Int -> m ()
checkCoverage x = traverse_ coverIn $ enumFromThenTo 0 step 90
 where
  step = 10
  coverIn lb = let ub = lb + step in classify ("[" <> fromString (show lb) <> "," <> fromString (show ub) <> "]") $ x > lb && x <= ub

prop_testcoverage :: Property
prop_testcoverage = property $ do
  x <- forAll $ int $ linearFrom 50 0 100
  cover 0 "[0,20)" $ 0 <= x && x < 20
  cover 10 "[20,20)" $ 20 <= x && x < 40
  cover 30 "[40,60)" $ 40 <= x && x < 60
  cover 10 "[60,80)" $ 60 <= x && x < 80
  cover 0 "[80,100)" $ 80 <= x && x < 100

prop_shrinktest :: Property
prop_shrinktest = property $ do
  x <- forAll $ int $ linear 0 1000
  collect x

prop_test_tripping :: Property
prop_test_tripping =
  property $ do
    x <- forAll $ int (linear 1 1000)
    let readMaybe :: String -> Either String Int
        readMaybe s = case reads s of [(a, "")] -> Right a; _ -> Left "ERROR"
    readMaybe (show x) === pure x
    tripping x show readMaybe

data Value a = Value a | ValueGroup [Value a] deriving (Show, Eq)

singleValue :: Gen (Value Int)
singleValue = Value <$> int (linear 0 1000)
anyValue :: Gen (Value Int)
anyValue =
  recursive
    ((ValueGroup <$>) . subsequence <=< sequence)
    (replicate 10 singleValue)
    [anyValue]

prop_recursive :: Property
prop_recursive = property $ do
  let
    groupValue (ValueGroup v) = v
    groupValue _ = []
    groupDepth :: Value a -> Int
    groupDepth x = case x of
      Value _ -> 0
      ValueGroup v -> sum (groupDepth <$> v) + 1
  x <- forAll anyValue
  label (fromString $ "number of values in top level:" <> show (length $ groupValue x))
  label (fromString $ "max group depth:" <> show (groupDepth x))

sampleMany :: (Show a) => Gen a -> IO ()
sampleMany gen = seeds >>= traverse_ (\s -> printWith 99 s gen)
 where
  seeds = replicateM 100 random
