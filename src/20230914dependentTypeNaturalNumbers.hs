#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.Type.Equality hiding (type (==))
import GHC.Natural
import Test.Hspec
import Prelude hiding (pred, succ)
import Data.Kind (Type)

data Nat = Z | S Nat deriving (Eq, Show)

genSingletons [''Nat]

type SomeNat = SomeSing Nat

pattern SomeNat :: SNat (n :: Nat) -> SomeSing Nat
pattern SomeNat n = SomeSing n

{-# COMPLETE SomeNat #-}

instance Eq (SNat n) where _ == _ = True

instance Show (SNat n) where show = showSNat

instance SDecide Nat where
  (%~) :: SNat n -> SNat m -> Decision (n :~: m)
  SZ %~ SZ = Proved Refl
  (SS m) %~ (SS n) = case m %~ n of
    Proved Refl -> Proved Refl
    Disproved f -> Disproved $ \Refl -> f Refl
  (SS _) %~ SZ = Disproved $ \case {}
  SZ %~ (SS _) = Disproved $ \case {}

allNats :: [SomeNat]
allNats = iterate (\(SomeNat n) -> SomeNat (SS n)) (SomeNat SZ)

someNat :: Natural -> SomeNat
someNat n = allNats !! fromIntegral n

toNatural :: SNat n -> Natural
toNatural SZ = 0
toNatural (SS x) = 1 + toNatural x

showSNat :: SNat n -> String
showSNat x = "SNat @" <> show (toNatural x)

showSomeSNat :: SomeNat -> String
showSomeSNat (SomeNat x) = showSNat x

suc :: SNat n -> SNat ('S n)
suc SZ = SS SZ
suc n@(SS _) = SS n

pred :: (n ~ 'S m) => SNat n -> SNat m
pred (SS n) = n

isEqual :: SNat n -> SNat m -> Decision (n :~: m)
isEqual SZ SZ = Proved Refl
isEqual (SS n) (SS m) = case isEqual n m of
  Proved Refl -> Proved Refl
  Disproved f -> Disproved $ \Refl -> f Refl
isEqual SZ (SS _) = Disproved $ \case {}
isEqual (SS _) SZ = Disproved $ \case {}

-- ============================== Arithmetic ==============================

type (+) :: Nat -> Nat -> Nat
type family (+) n m where
  'Z + n = n -- (SA)
  ('S n) + m = 'S (n + m) -- (SB)

type (-) :: Nat -> Nat -> Nat
type family (-) n m where
  n - 'Z = n
  ('S n) - ('S m) = n - m

sumFlipZ :: forall n. SNat n -> n + 'Z :~: n
-- 0 + 0      =     0                     by (SA)
sumFlipZ SZ = Refl
-- n + 0      =     (1 + (n - 1)) + 0     by input pattern
--            =     1 + ((n - 1) + 0)     by (SB)
--            =     1 + (n - 1)           by recursion
--            =     n
sumFlipZ (SS n_) = gcastWith (sumFlipZ n_) Refl

sumFlipS :: forall m n. SNat n -> SNat m -> n + 'S m :~: 'S n + m
-- 0 + (1 + m)      =     1 + m                       by (SA)
--                  =     1 + (0 + m)                 by (SA)
--                  =     (1 + 0) + m                 by (SB)
sumFlipS SZ _ = Refl
-- n + (1 + m)      =     (1 + (n - 1)) + (1 + m)     by input pattern
--                  =     1 + ((n - 1) + (1 + m))     by (SB)
--                  =     1 + (n + m)                 by recursion
--                  =     (1 + n) + m                 by (SB)
sumFlipS (SS n_) m = gcastWith (sumFlipS n_ m) Refl

isAddCommutative' :: forall n m. (SingI n, SingI m) => (n + m :~: m + n)
isAddCommutative' = isAddCommutative (sing @n) (sing @m)

isAddCommutative :: forall n m. SNat n -> SNat m -> (n + m :~: m + n)
-- 0 + m    =     m                       by (SA)
--          =     m + 0                   by sumFlipZ
isAddCommutative SZ m = gcastWith (sumFlipZ m) Refl
-- n + m    =     (1 + (n - 1)) + m       by pattern match
--          =     1 + ((n - 1) + m)       by (SB)
--          =     1 + (m + (n - 1))       by recursion
--          =     (1 + m) + (n - 1)       by (SB)
--          =     m + n                   by sumFlipS
isAddCommutative (SS (n_ :: SNat n_)) m =
  gcastWith (isAddCommutative n_ m :: n_ + m :~: m + n_) $
    gcastWith (sumFlipS m n_ :: m + n :~: 'S (m + n_)) Refl

isAddAssociative' :: forall n m o. (SingI n, SingI m, SingI o) => (n + m) + o :~: n + (m + o)
isAddAssociative' = isAddAssociative (sing @n) (sing @m) (sing @o)

isAddAssociative :: forall n m o. SNat n -> SNat m -> SNat o -> ((n + m) + o :~: n + (m + o))
-- (0 + m) + n      =     m + n                         by (SA)
--                  =     (m + n)
--                  =     0 + (m + n)                   by (SA)
isAddAssociative SZ _ _ = Refl
-- (n + m) + o      =     ((1 + (n - 1)) + m) + o       by pattern match
--                  =     (1 + ((n - 1) + m) + o        by (SB)
--                  =     1 + (((n - 1) + m) + o)       by (SB)
--                  =     1 + ((n - 1) + (m + o))       by recursion
--                  =     n + (m + o)                   by (SB)
isAddAssociative (SS (n_ :: SNat n_)) m o = gcastWith (isAddAssociative n_ m o) Refl

repeatN :: forall f n m. (forall (a :: Nat). SNat a -> f a -> f ('S a)) -> SNat n -> SNat m -> f (m :: Nat) -> f (m + n)
-- f m                    =     f (0 + m)                 by (SA)
--                        =     f (m + 0)                 by sumFlipZ
repeatN _ SZ m x = gcastWith (sumFlipZ m) x
-- g (f (m + (n - 1)))    =     f (1 + (m + (n - 1)))     by g
--                        =     f ((1 + m) + (n - 1))     by (SB)
--                        =     f (m + n)                 by sumFlipS
repeatN g (SS n_) m x = repeatN g n_ m x & g (m %+ n_) & gcastWith (sumFlipS m n_)

unrepeatN :: (forall (a :: Nat). SNat a -> f ('S a) -> f a) -> SNat n -> SNat m -> f (m :: Nat) -> Maybe (f (m - n))
unrepeatN _ SZ _ x = Just x
unrepeatN f (SS n_) (SS m_) x = unrepeatN f n_ m_ $ f m_ x
unrepeatN _ _ SZ _ = Nothing

(%+) :: SNat n -> SNat m -> SNat (n + m)
SZ %+ x = x
(SS n) %+ x = SS $ n %+ x

(%-) :: SNat n -> SNat m -> Maybe (SNat (n - m))
x %- SZ = Just x
(SS n) %- (SS m) = n %- m
_ %- _ = Nothing

add2 :: SNat n -> SNat m -> SNat (n + m)
add2 x y = gcastWith (isAddCommutative x y) $ repeatN (const SS) x y y

add3 :: SNat n -> SNat m -> SNat o -> SNat (n + m + o)
add3 n m o = gcastWith (isAddAssociative n m o) $ add2 n (add2 m o)

-- ============================== Test ==============================

type Nat0 = 'Z

type Nat1 = 'S Nat0

type Nat2 = 'S Nat1

type Nat3 = 'S Nat2

type Nat4 = 'S Nat3

type Nat5 = 'S Nat4

type Nat10 = Nat5 + Nat5

snat0 :: SNat Nat0
snat0 = SZ

snat1 :: SNat Nat1
snat1 = suc snat0

snat2 :: SNat Nat2
snat2 = suc snat1

snat3 :: SNat Nat3
snat3 = suc snat2

snat4 :: SNat Nat4
snat4 = SS snat3

snat5 :: SNat Nat5
snat5 = SS snat4

snat10 :: SNat Nat10
snat10 = snat5 %+ snat5

snat20 :: SNat (Nat10 + Nat10)
snat20 = snat10 %+ snat10

main :: IO ()
main = do
  traverse_
    (putStrLn . showSomeSNat)
    [ SomeNat snat0,
      SomeNat snat1,
      SomeNat snat2
    ]

  print $ showSNat snat10
  print $ showSNat snat20
  hspec spec

spec :: SpecWith ()
spec = describe "Nat" $ do
  let expectNatsEqual :: SNat n -> SNat m -> IO ()
      expectNatsEqual n m = case isEqual n m of
        Proved Refl -> n `shouldBe` m
        Disproved _ -> expectationFailure $ show n <> " != " <> show m
      expectSomeNatsEqual (SomeNat n) (SomeNat m) = expectNatsEqual n m
      testSomeNatsMaybe (Just x) (Just y) = expectSomeNatsEqual x y
      testSomeNatsMaybe Nothing Nothing = pure ()
      testSomeNatsMaybe _ _ = expectationFailure ""
      expectProof :: Decision a -> Bool -> String -> IO ()
      expectProof dec expectProved message = case dec of
        Proved _ -> unless expectProved $ expectationFailure $ "Expected disproof:" <> message
        Disproved _ -> when expectProved $ expectationFailure $ "Expected proof: " <> message

  it "setup" $ do
    expectSomeNatsEqual (someNat 0) (SomeNat snat0)
    expectSomeNatsEqual (someNat 1) (SomeNat snat1)
    expectSomeNatsEqual (someNat 2) (SomeNat snat2)

  it "[toNatural]" $ do
    let test (SomeNat n) x = toNatural n `shouldBe` x
    traverse_
      (uncurry test)
      [ (SomeNat snat0, 0),
        (SomeNat snat1, 1),
        (SomeNat snat2, 2),
        (SomeNat snat3, 3),
        (SomeNat snat4, 4),
        (SomeNat snat5, 5),
        (SomeNat snat10, 10),
        (SomeNat snat20, 20)
      ]

  it "[isEqual]" $ do
    let test :: (SomeNat, SomeNat, Bool) -> IO ()
        test (SomeNat n, SomeNat m, expectProved) = expectProof (isEqual n m) expectProved (show n <> "," <> show m)

    traverse_
      test
      [ (someNat 0, someNat 0, True),
        (someNat 0, someNat 1, False),
        (someNat 0, someNat 2, False),
        (someNat 1, someNat 0, False),
        (someNat 1, someNat 1, True),
        (someNat 1, someNat 2, False),
        (someNat 2, someNat 0, False),
        (someNat 2, someNat 1, False),
        (someNat 2, someNat 2, True)
      ]

    x <-
      traverse (\(n, m) -> test (someNat n, someNat m, m == n)) $
        (,) <$> [0 .. 20] <*> [0 .. 20]
    length x `shouldBe` 441

  describe "[+]" $ do
    it "basic" $ do
      traverse_
        (uncurry expectSomeNatsEqual)
        [ (SomeNat (snat0 %+ snat0), someNat 0),
          (SomeNat (snat0 %+ snat1), someNat 1),
          (SomeNat (snat0 %+ snat2), someNat 2),
          (SomeNat (snat0 %+ snat3), someNat 3),
          (SomeNat (snat1 %+ snat0), someNat 1),
          (SomeNat (snat1 %+ snat1), someNat 2),
          (SomeNat (snat1 %+ snat2), someNat 3),
          (SomeNat (snat1 %+ snat3), someNat 4),
          (SomeNat (snat2 %+ snat0), someNat 2),
          (SomeNat (snat2 %+ snat1), someNat 3),
          (SomeNat (snat2 %+ snat2), someNat 4),
          (SomeNat (snat2 %+ snat3), someNat 5)
        ]

      let testSum (SomeNat n, SomeNat m, SomeNat x) = expectNatsEqual (n %+ m) x
      x <-
        traverse
          (\(n, m) -> testSum (someNat n, someNat m, someNat (n + m)))
          $ (,) <$> [0 .. 100] <*> [0 .. 100]

      length x `shouldBe` 10201
      void $ return x

    it "[isAddCommutative]" $ do
      (isAddCommutative' @Nat0 @Nat1) `shouldBe` Refl
      (isAddCommutative' @Nat1 @Nat0) `shouldBe` Refl
      (isAddCommutative' @Nat1 @Nat2) `shouldBe` Refl
      (isAddCommutative' @Nat2 @Nat1) `shouldBe` Refl

    it "[isAddCommutative] application" $ do
      x <-
        traverse
          ( \(SomeNat n, SomeNat m) -> do
              let x = n %+ m
              expectSomeNatsEqual (SomeNat x) (SomeNat (add2 n m))
              expectSomeNatsEqual (SomeNat x) (SomeNat (add2 m n))
          )
          $ (,) <$> take 30 allNats <*> take 100 allNats
      length x `shouldBe` 3000

    it "[isAddAssociative]" $ do
      (isAddAssociative' @Nat0 @Nat1 @Nat3) `shouldBe` Refl
      (isAddAssociative' @Nat1 @Nat0 @Nat3) `shouldBe` Refl
      (isAddAssociative' @Nat1 @Nat3 @Nat0) `shouldBe` Refl
      (isAddAssociative' @Nat1 @Nat3 @Nat5) `shouldBe` Refl

    it "[isAddAssociative] application" $ do
      x <-
        traverse
          ( \(SomeNat n, SomeNat m, SomeNat o) -> do
              let x = add3 n m o
              expectSomeNatsEqual (SomeNat ((n %+ m) %+ o)) (SomeNat x)
              expectSomeNatsEqual (SomeNat (n %+ (m %+ o))) (SomeNat x)
          )
          $ (,,) <$> take 20 allNats <*> take 20 allNats <*> take 20 allNats
      length x `shouldBe` 8000

  describe "[-]" $ do
    it "basic" $ do
      traverse_
        (uncurry testSomeNatsMaybe)
        [ (SomeNat <$> (snat0 %- snat0), Just (someNat 0)),
          (SomeNat <$> (snat0 %- snat1), Nothing),
          (SomeNat <$> (snat0 %- snat2), Nothing),
          (SomeNat <$> (snat0 %- snat3), Nothing),
          (SomeNat <$> (snat1 %- snat0), Just (someNat 1)),
          (SomeNat <$> (snat1 %- snat1), Just (someNat 0)),
          (SomeNat <$> (snat1 %- snat2), Nothing),
          (SomeNat <$> (snat1 %- snat3), Nothing),
          (SomeNat <$> (snat2 %- snat0), Just (someNat 2)),
          (SomeNat <$> (snat2 %- snat1), Just (someNat 1)),
          (SomeNat <$> (snat2 %- snat2), Just (someNat 0)),
          (SomeNat <$> (snat2 %- snat3), Nothing)
        ]

data List a = Empty | Succ a (List a) Int

pattern MyEmptyList :: List a
pattern MyEmptyList = Empty

pattern MySucc :: a -> List a -> List a
pattern MySucc a l <- Succ a l n
-- pattern MySucc a l <- Succ a (Succ _ l _) n
-- pattern MySucc a l <- Succ a (Succ _ l _) n | l' = l
  where
    MySucc a l
      | Succ _ _ n <- l = Succ a l (n + 1)
      | Empty <- l = Succ a Empty 1
