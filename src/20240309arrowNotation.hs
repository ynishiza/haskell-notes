#!/usr/bin/env stack
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note

Ref: https://www.haskell.org/arrows/biblio.html
Paper: "Programming with arrows"

Section 3 "Pointed arrow programming"

Ref: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html
-}
module NoteArrowNotation (
  SF (..),
  main,
) where

import Control.Arrow
import Control.Category
import Data.Either
import Data.Function hiding (id, (.))
import Debug.Trace (trace)
import Test.Hspec
import Prelude hiding (id, (.))
import Prelude qualified

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

myTrace :: String -> a -> a
myTrace = trace

spec :: Spec
spec = describe "" $ do
  let
    addStream :: SF Int Int
    addStream = arr (+ 1)

  describe "SF" $ do
    it "basic" $ do
      runSF id [1 :: Int .. 10] `shouldBe` [1 .. 10]
      runSF addStream [1 .. 10] `shouldBe` [2 .. 11]

    it "[ArrowChoice]" $ do
      let
        x = [Left (1 :: Int), Right (), Left 3]
      runSF (left addStream) x `shouldBe` [Left 2, Right (), Left 4]
      runSF (left @SF @Int @Int @() addStream) [] `shouldBe` []

      runSF (addStream +++ arrConst 'a') x `shouldBe` [Left 2, Right 'a', Left 4]

    it "[mapA]" $ do
      runSF (mapA addStream) [[1, 2], [3, 4], [5], [6, 7, 8]] `shouldBe` [[2, 3], [4, 5], [6], [7, 8, 9]]
      runSF (mapA (arr id)) [[1 :: Int, 2], [3], [4, 5]] `shouldBe` [[1, 2], [3], [4, 5]]

    let sample =
          [ [1 :: Int, 2, 3]
          , [4, 5]
          , [6]
          , [7, 8]
          , [9, 10, 11]
          , [12, 13, 14, 15]
          ]

    it "[delayA]" $ do
      runSF delayA [[1 :: Int, 2, 3], [4, 5, 6], [7, 8, 9]]
        `shouldBe` [ [1]
                   , [4, 2]
                   , [7, 5, 3]
                   ]
      runSF delayA sample
        `shouldBe` [ [1]
                   , [4, 2]
                   , [6, 5, 3]
                   , [7]
                   , [9, 8]
                   , [12, 10]
                   ]

      -- expectationFailure "WHAT IS GOING ON?"

    it "[delay]" $ do
      let p = mapA (delay 0 >>> arrTrace "delay 0:":: SF Int Int) :: SF [Int] [Int]
      runSF (delay 0) [1 :: Int, 2, 3] `shouldBe` [0, 1, 2]
      runSF p [[1 :: Int, 2, 3], [4, 5, 6]] `shouldBe` [[0, 0, 0], [1, 2, 3]]
      putStrLn "============================================"
      runSF p sample
        `shouldBe` [ [0, 0, 0]
                   , [1, 2]
                   , [4]
                   , [6, 5]
                   , [7, 8, 3]
                   , [9, 10, 11, 0]
                   ]

      -- expectationFailure "WHAT IS GOING ON?"

-- ============================== SF ==============================

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Category SF where
  id = SF Prelude.id
  SF f . SF g = SF $ g >>> f

instance Arrow SF where
  arr f = SF (f <$>)
  first (SF f) = SF $ unzip >>> first f >>> uncurry zip

instance ArrowChoice SF where
  left (SF f) = SF $ \xs -> replaceLeft xs (f (lefts xs))
   where
    replaceLeft (Left _ : xs) (z : zs) = Left z : replaceLeft xs zs
    replaceLeft (Left _ : xs) [] = replaceLeft xs []
    replaceLeft (Right x : xs) zs = Right x : replaceLeft xs zs
    replaceLeft [] _ = []

arrTrace :: (Show x) => (Arrow a) => String -> a x x
arrTrace name = arr $ \x -> myTrace (name <> show x) x

arrConst :: (Arrow a) => x -> a b x
arrConst = arr . const

listcase :: (Show a) => [a] -> Either () (a, [a])
listcase [] = Left ()
listcase (x : xs) = trace ("listcase x=" <> show x) $ Right (x, xs)

mapA :: forall a b c. (Show b, Show c) => (ArrowChoice a) => a b c -> a [b] [c]
mapA f =
  arr @a listcase
    >>> (arrConst [] +++ applyMap)
    >>> arr (either id (uncurry (:)))
 where
  applyMap = (arrTrace "f before:" >>> f >>> arrTrace "f after:") *** mapA f >>> arrTrace "mapA"

-- delayA :: ArrowChoice a => SF b b
delayA :: (Show a) => SF [a] [a]
delayA =
  arr listcase
    >>> (arrConst [] ||| (f >>> arr (uncurry (:))))
 where
  f = arr id *** (delayA >>> delay [])

-- >>> (id ||| (f *** mapA f))

delay :: a -> SF a a
delay x = SF $ init . (x :)

-- delay x = SF  (x :)
