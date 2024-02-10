#!/usr/bin/env stack
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Evaluate" #-}

{- | Scrap

Scrap your type classes: https://www.haskellforall.com/2012/05/scrap-your-type-classes.html
-}
module NoteScrapYouTypeClasses where

import Data.Coerce
import Data.Foldable
import Data.Monoid
import Test.Hspec

-- * Section

--

{- $info
a
-}

main :: IO ()
main = do
  hspec spec
  return ()

data MonadI m = MonadI
  { _monadFunctor :: FunctorI m
  , _return :: forall a. a -> m a
  , _bind :: forall a b. m a -> (a -> m b) -> m b
  }

data FunctorI f = FunctorI
  { _fmap :: forall a b. (a -> b) -> f a -> f b
  }

data MonoidI a = MonoidI
  { _mempty :: a
  , _mappend :: a -> a -> a
  }

functorMaybe :: FunctorI Maybe
functorMaybe =
  FunctorI
    { _fmap = \f x -> maybe Nothing (Just . f) x
    }

monadMaybe :: MonadI Maybe
monadMaybe =
  MonadI
    { _monadFunctor = functorMaybe
    , _return = Just
    , _bind = \x k -> maybe Nothing k x
    }

sumMonoid :: (Num a) => MonoidI a
sumMonoid = MonoidI 0 (+)

prodMonoid :: (Num a) => MonoidI a
prodMonoid = MonoidI 1 (*)

listMonoid :: MonoidI [a]
listMonoid = MonoidI [] (++)

endoMonoid :: MonoidI (a -> a)
endoMonoid = MonoidI id (.)

dualMonoid :: MonoidI a -> MonoidI a
dualMonoid (MonoidI{..}) =
  MonoidI
    { _mempty = _mempty
    , _mappend = flip _mappend
    }

-- | fold :: Traversable t, Monoid m => t a -> a
_fold :: (Foldable t) => MonoidI m -> t m -> m
_fold (MonoidI{..}) = foldl' _mappend _mempty

spec :: Spec
spec = describe "" $ do
  describe "Monad" $ do
    it "Maybe" $ do
      _fmap (_monadFunctor monadMaybe) (* 2) (Just 1) `shouldBe` (* 2) <$> Just (1 :: Int)
      _fmap (_monadFunctor monadMaybe) (* (2 :: Int)) Nothing `shouldBe` (* 2) <$> Nothing

      _return monadMaybe "A" `shouldBe` Just "A"
      _bind monadMaybe (Just "a") (Just . (<> "hello")) `shouldBe` (Just "a" >>= (Just . (<> "hello")))
      _bind monadMaybe Nothing (Just . (<> "hello")) `shouldBe` (Nothing >>= (Just . (<> "hello")))

  describe "Monoid" $ do
    it "sum, product" $ do
      _mappend @Int prodMonoid 1 2 `shouldBe` 1 * 2
      _mappend @Int sumMonoid 1 2 `shouldBe` 1 + 2
      _fold (sumMonoid @Int) [1 .. 10] `shouldBe` coerce (foldMap Sum [1 .. 10 :: Int])
      _fold (prodMonoid @Int) [1 .. 10] `shouldBe` coerce (foldMap Product [1 .. 10 :: Int])

    it "endo" $ do
      _fold endoMonoid [(* 2), (+ 10)] 1 `shouldBe` appEndo (foldMap (Endo @Int) [(* 2), (+ 10)]) 1

    it "list" $ do
      _mappend listMonoid "hello" "a" `shouldBe` "helloa"

    it "dual" $ do
      _mappend listMonoid "hello" "a" `shouldBe` "helloa"
      _mappend (dualMonoid listMonoid) "hello" "a" `shouldBe` "ahello"

      let ops :: [Int -> Int] = [(* 2), (+ 1)]
      _fold (endoMonoid @Int) ops 0 `shouldBe` 2
      _fold (endoMonoid @Int) ops 0 `shouldBe` appEndo (foldMap (Endo @Int) ops) 0
      _fold (endoMonoid @Int) (reverse ops) 0 `shouldBe` 1
      _fold (endoMonoid @Int) (reverse ops) 0 `shouldBe` appEndo (foldMap (Endo @Int) (reverse ops)) 0
      _fold (dualMonoid $ endoMonoid @Int) ops 0 `shouldBe` 1
      _fold (dualMonoid $ endoMonoid @Int) ops 0 `shouldBe` (appEndo $ getDual (foldMap (Dual . (Endo @Int)) ops)) 0
      _fold (dualMonoid $ endoMonoid @Int) (reverse ops) 0 `shouldBe` 2
      _fold (dualMonoid $ endoMonoid @Int) (reverse ops) 0 `shouldBe` (appEndo $ getDual (foldMap (Dual . (Endo @Int)) (reverse ops))) 0
