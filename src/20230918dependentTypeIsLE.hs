#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
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
import Data.Kind (Type)
import Data.Singletons.TH
import Data.Type.Equality (gcastWith)
import Data.Void
import GHC.Natural (Natural)
import Test.Hspec

data Nat = Z | S Nat deriving (Eq, Show)

type Neg a = a -> Void

genSingletons [''Nat]

instance Show (SNat n) where show = showSNat

type SomeNat = SomeSing Nat

pattern SomeNat :: SNat (n :: Nat) -> SomeSing Nat
pattern SomeNat n = SomeSing n

{-# COMPLETE SomeNat #-}

showSNat :: SNat n -> String
showSNat x = "SNat @" <> show (toNatural x)

toNatural :: SNat n -> Natural
toNatural SZ = 0
toNatural (SS x) = 1 + toNatural x

data SomeLEProof where
  SomeLEProof :: forall m n. (SingI m, SingI n) => (LEProof m n) -> SomeLEProof

type LEProof :: Nat -> Nat -> Type
data LEProof m n where
  LEZero :: LEProof 'Z n -- witness: 0 <= x for any x
  LESucc :: LEProof m n -> LEProof ('S m) ('S n) -- witness: x <= y implies x+1 <= y+1

instance Eq (LEProof x y) where _ == _ = True

instance (SingI x, SingI y) => Show (LEProof x y) where show = showLEProof

showLEProof :: forall m n. (SingI m, SingI n) => LEProof m n -> String
showLEProof _ = "LEProof: " <> showSNat (sing @m) <> " <= " <> showSNat (sing @n)

showSomeLEProof :: SomeLEProof -> String
showSomeLEProof (SomeLEProof x) = showLEProof x

decideLE :: forall m n. (SingI m, SingI n) => Decision (LEProof m n)
decideLE = decideLE' (sing @m) (sing @n)

decideLE' :: SNat m -> SNat n -> Decision (LEProof m n)
decideLE' SZ _ = Proved LEZero
decideLE' (SS m) (SS n) = case decideLE' m n of
  Proved proof -> Proved $ LESucc proof
  Disproved f -> Disproved $ \case
    LESucc proof -> f proof
decideLE' _ _ = Disproved $ \case {}

leToNat :: forall m n. (SingI m, SingI n) => LEProof m n -> (SNat m, SNat n)
leToNat _ = (sing @m, sing @n)

leZero :: LEProof 'Z n
leZero = LEZero

leRefl :: forall n. SingI n => LEProof n n
leRefl = case sing @n of
  SZ -> LEZero
  SS x -> withSingI x leRefl

leSucc :: LEProof m n -> LEProof ('S m) ('S n)
leSucc = LESucc

lePred :: LEProof ('S m) ('S n) -> LEProof m n
lePred (LESucc proof) = proof

leStep :: LEProof m n -> LEProof m ('S n)
leStep LEZero = LEZero
leStep (LESucc proof) = LESucc $ leStep proof

leStepL :: LEProof ('S m) n -> LEProof m n
leStepL x = let LESucc y = leStep x in y

leAsym :: LEProof m n -> LEProof n m -> n :~: m
leAsym LEZero LEZero = Refl
leAsym (LESucc m_) (LESucc n_) = gcastWith (leAsym m_ n_) Refl

leTrans :: LEProof m n -> LEProof n o -> LEProof m o
leTrans LEZero _ = LEZero
leTrans (LESucc m_) (LESucc n_) = leSucc $ leTrans m_ n_

proofZeroLEZero :: LEProof n 'Z -> n :~: 'Z
proofZeroLEZero LEZero = Refl

leSwap :: SNat m -> SNat n -> Neg (LEProof m n) -> LEProof ('S n) m
leSwap (SS m_) (SS n_) f = LESucc $ leSwap m_ n_ (f . LESucc)
leSwap SZ _ f = absurd $ f LEZero
leSwap (SS _) SZ _ = LESucc LEZero

leSwap' :: LEProof n m -> LEProof ('S m) n -> Void
leSwap' = undefined

type Nat0 = 'Z

type Nat1 = 'S Nat0

type Nat2 = 'S Nat1

type Nat3 = 'S Nat2

type Nat4 = 'S Nat3

type Nat5 = 'S Nat4

snat0 :: SNat Nat0
snat0 = SZ

snat1 :: SNat Nat1
snat1 = SS snat0

snat2 :: SNat Nat2
snat2 = SS snat1

snat3 :: SNat Nat3
snat3 = SS snat2

snat4 :: SNat Nat4
snat4 = SS snat3

snat5 :: SNat Nat5
snat5 = SS snat4

zeroLEZero :: LEProof Nat0 Nat0
zeroLEZero = LEZero @'Z

zeroLEOne :: LEProof Nat0 Nat1
zeroLEOne = LEZero

zeroLETwo :: LEProof Nat0 Nat2
zeroLETwo = LEZero

zeroLEThree :: LEProof Nat0 Nat3
zeroLEThree = LEZero

oneLEOne :: LEProof Nat1 Nat1
oneLEOne = LESucc zeroLEZero

oneLETwo :: LEProof Nat1 Nat2
oneLETwo = LESucc zeroLEOne

oneLEThree :: LEProof Nat1 Nat3
oneLEThree = LESucc zeroLETwo

twoLETwo :: LEProof Nat2 Nat2
twoLETwo = LESucc oneLEOne

twoLEThree :: LEProof Nat2 Nat3
twoLEThree = LESucc oneLETwo

main :: IO ()
main = do
  putStrLn "Hello"
  putStrLn $ showSomeLEProof $ SomeLEProof zeroLEZero
  putStrLn $ showSomeLEProof $ SomeLEProof oneLEOne
  hspec spec

allNats :: [SomeNat]
allNats = iterate (\(SomeNat n) -> SomeNat (SS n)) (SomeNat SZ)

spec :: SpecWith ()
spec = describe "LEProof" $ do
  let expectProof :: Decision a -> Bool -> String -> IO ()
      expectProof dec expectProved message = case dec of
        Proved _ -> unless expectProved $ expectationFailure $ "Expected disproof:" <> message
        Disproved _ -> when expectProved $ expectationFailure $ "Expected proof: " <> message

  it "basic" $ do
    proofZeroLEZero zeroLEZero `shouldBe` Refl

  it "[decideLE']" $ do
    let test (SomeNat m) (SomeNat n) = expectProof (decideLE' m n) (toNatural m <= toNatural n) $ show m <> "<=" <> show n
    x <- traverse (uncurry test) $ (,) <$> take 20 allNats <*> take 20 allNats
    length x `shouldBe` 400

  it "[lePred]" $ do
    lePred oneLEOne `shouldBe` zeroLEZero
    lePred oneLETwo `shouldBe` zeroLEOne
    lePred twoLETwo `shouldBe` oneLEOne
    lePred twoLEThree `shouldBe` oneLETwo

  it "[leStep]" $ do
    leStep zeroLEZero `shouldBe` zeroLEOne
    leStep zeroLEOne `shouldBe` zeroLETwo
    leStep oneLEOne `shouldBe` oneLETwo
    leStep oneLETwo `shouldBe` oneLEThree
    leStep twoLETwo `shouldBe` twoLEThree

  it "[leStepL]" $ do
    leStepL oneLEOne `shouldBe` zeroLEOne
    leStepL oneLETwo `shouldBe` zeroLETwo
    leStepL twoLETwo `shouldBe` oneLETwo
    leStepL twoLEThree `shouldBe` oneLEThree

  it "[leAsym]" $ do
    leAsym zeroLEZero zeroLEZero `shouldBe` Refl
    leAsym oneLEOne oneLEOne `shouldBe` Refl

  it "[leTrans]" $ do
    leTrans zeroLEZero zeroLEZero `shouldBe` zeroLEZero
    leTrans zeroLEZero zeroLEOne `shouldBe` zeroLEOne
    leTrans zeroLEZero zeroLETwo `shouldBe` zeroLETwo
    leTrans zeroLEOne oneLEOne `shouldBe` zeroLEOne
    leTrans zeroLEOne oneLETwo `shouldBe` zeroLETwo
    leTrans zeroLETwo twoLETwo `shouldBe` zeroLETwo
    leTrans oneLEOne oneLEOne `shouldBe` oneLEOne
    leTrans oneLEOne oneLETwo `shouldBe` oneLETwo
    leTrans oneLETwo twoLETwo `shouldBe` oneLETwo
    leTrans oneLETwo twoLEThree `shouldBe` oneLEThree
    leTrans twoLETwo twoLETwo `shouldBe` twoLETwo
    leTrans twoLETwo twoLEThree `shouldBe` twoLEThree

  it "[leSwap]" $ do
    leSwap snat1 snat0 (\case {}) `shouldBe` oneLEOne
    leSwap snat2 snat1 (\(LESucc x) -> case x of {}) `shouldBe` twoLETwo
