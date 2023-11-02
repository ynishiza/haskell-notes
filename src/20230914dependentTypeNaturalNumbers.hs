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
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TH (genSingletons)
import Data.Type.Equality hiding (type (==))
import GHC.Natural
import Test.Hspec
import Prelude hiding (pred, succ)

data Nat = Z | S Nat deriving (Eq, Show)

genSingletons [''Nat]

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

toNatural :: SNat n -> Natural
toNatural SZ = 0
toNatural (SS x) = 1 + toNatural x

showSNat :: SNat n -> String
showSNat x = "SNat @" <> show (toNatural x)

suc :: SNat n -> SNat ('S n)
suc SZ = SS SZ
suc n@(SS _) = SS n

pred :: (n ~ 'S m) => SNat n -> SNat m
pred (SS n) = n

-- ============================== Opaque type ==============================

type SomeNat = SomeSing Nat

pattern SomeNat :: SNat (n :: Nat) -> SomeSing Nat
pattern SomeNat n = SomeSing n

showSomeSNat :: SomeNat -> String
showSomeSNat (SomeNat x) = showSNat x

{-# COMPLETE SomeNat #-}

allNats :: [SomeNat]
allNats = iterate (\(SomeNat n) -> SomeNat (SS n)) (SomeNat SZ)

someNat :: Natural -> SomeNat
someNat n = allNats !! fromIntegral n

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

sumIsCommutative' :: forall n m. (SingI n, SingI m) => (n + m :~: m + n)
sumIsCommutative' = sumIsCommutative (sing @n) (sing @m)

sumIsCommutative :: forall n m. SNat n -> SNat m -> (n + m :~: m + n)
-- 0 + m    =     m                       by (SA)
--          =     m + 0                   by sumFlipZ
sumIsCommutative SZ m = gcastWith (sumFlipZ m) Refl
-- n + m    =     (1 + (n - 1)) + m       by pattern match
--          =     1 + ((n - 1) + m)       by (SB)
--          =     1 + (m + (n - 1))       by recursion
--          =     (1 + m) + (n - 1)       by (SB)
--          =     m + n                   by sumFlipS
sumIsCommutative (SS (n_ :: SNat n_)) m =
  gcastWith (sumIsCommutative n_ m :: n_ + m :~: m + n_)
    $ gcastWith (sumFlipS m n_ :: m + n :~: 'S (m + n_)) Refl

sumIsAssociative' :: forall n m o. (SingI n, SingI m, SingI o) => (n + m) + o :~: n + (m + o)
sumIsAssociative' = sumIsAssociative (sing @n) (sing @m) (sing @o)

sumIsAssociative :: forall n m o. SNat n -> SNat m -> SNat o -> ((n + m) + o :~: n + (m + o))
-- (0 + m) + n      =     m + n                         by (SA)
--                  =     (m + n)
--                  =     0 + (m + n)                   by (SA)
sumIsAssociative SZ _ _ = Refl
-- (n + m) + o      =     ((1 + (n - 1)) + m) + o       by pattern match
--                  =     (1 + ((n - 1) + m) + o        by (SB)
--                  =     1 + (((n - 1) + m) + o)       by (SB)
--                  =     1 + ((n - 1) + (m + o))       by recursion
--                  =     n + (m + o)                   by (SB)
sumIsAssociative (SS (n_ :: SNat n_)) m o = gcastWith (sumIsAssociative n_ m o) Refl

repeatN :: forall f n m. (forall (a :: Nat). SNat a -> f a -> f ('S a)) -> SNat n -> SNat m -> f (m :: Nat) -> f (m + n)
-- f m                    =     f (0 + m)                 by (SA)
--                        =     f (m + 0)                 by sumFlipZ
repeatN _ SZ m x = gcastWith (sumFlipZ m) x
-- g (f (m + (n - 1)))    =     f (1 + (m + (n - 1)))     by g
--                        =     f ((1 + m) + (n - 1))     by (SB)
--                        =     f (m + n)                 by sumFlipS
repeatN g (SS n_) m x =
  repeatN g n_ m x
    & g (m %+ n_)
    & gcastWith (sumFlipS m n_)

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
add2 x y =
  repeatN (const SS) x y y
    & gcastWith (sumIsCommutative x y)

add3 :: SNat n -> SNat m -> SNat o -> SNat (n + m + o)
add3 n m o =
  add2 n (add2 m o)
    & gcastWith (sumIsAssociative n m o)

-- ============================== Comparison ==============================

type (<=) :: Nat -> Nat -> Bool
type family (<=) m n where
  'Z <= n = 'True
  ('S m) <= ('S n) = m <= n
  _ <= _ = 'False

data IsLE m n where
  IsLE :: (m <= n ~ 'True) => SNat m -> SNat n -> IsLE m n

instance Eq (IsLE m n) where _ == _ = True

instance Show (IsLE m n) where show = showLEProof

showLEProof :: forall m n. IsLE m n -> String
showLEProof (IsLE m n) = "LEProof: " <> showSNat m <> " <= " <> showSNat n

decideLE :: SNat m -> SNat n -> Decision (IsLE m n)
decideLE SZ n = Proved $ IsLE SZ n
decideLE (SS m_) (SS n_) = case decideLE m_ n_ of
  Proved (IsLE _ _) -> Proved $ IsLE (SS m_) (SS n_)
  Disproved f -> Disproved $ \(IsLE _ _) -> f (IsLE m_ n_)
decideLE (SS _) SZ = Disproved $ \case {}

withIsLE :: IsLE m n -> ((m <= n ~ 'True) => r) -> r
withIsLE (IsLE _ _) f = f

isLEZero :: SNat n -> IsLE 'Z n
isLEZero n = IsLE SZ n

isLESucc :: IsLE m n -> IsLE ('S m) ('S n)
isLESucc (IsLE m n) = IsLE (SS m) (SS n)

isLEPred :: IsLE ('S m) ('S n) -> IsLE m n
isLEPred (IsLE (SS m_) (SS n_)) = IsLE m_ n_

isLEStep :: IsLE m n -> IsLE m ('S n)
isLEStep (IsLE SZ n) = IsLE SZ (SS n)
isLEStep (IsLE m@(SS m_) n@(SS n_)) = withIsLE (isLEStep (IsLE m_ n_)) $ IsLE m (SS n)

isLEStepL :: IsLE ('S m) n -> IsLE m n
isLEStepL (IsLE (SS SZ) n) = IsLE SZ n
isLEStepL (IsLE (SS (SS m_)) (SS n_)) = isLESucc (isLEStepL (IsLE (SS m_) n_))

isLETrans :: IsLE m n -> IsLE n o -> IsLE m o
isLETrans (IsLE SZ _) (IsLE _ o) = IsLE SZ o
isLETrans (IsLE (SS m_) (SS n_)) (IsLE _ (SS o_)) = isLESucc $ isLETrans (IsLE m_ n_) (IsLE n_ o_)

isLEAsym :: IsLE m n -> IsLE n m -> m :~: n
isLEAsym (IsLE SZ _) (IsLE SZ _) = Refl
isLEAsym (IsLE (SS m_) (SS n_)) (IsLE _ _) = gcastWith (isLEAsym (IsLE m_ n_) (IsLE n_ m_)) Refl

-- ============================== Elevator ==============================
--

data DoorState = DoorOpen | DoorClosed deriving (Show, Eq)

genSingletons [''DoorState]

instance Eq (SDoorState s) where _ == _ = True

instance Show (SDoorState s) where show = showDoorState

showDoorState :: SDoorState s -> String
showDoorState SDoorOpen = "DoorOpen"
showDoorState SDoorClosed = "DoorClosed"

data Elevator s n = Elevator (SDoorState s) (SNat n)

instance Eq (Elevator s n) where _ == _ = True

instance Show (Elevator s n) where show = showElevator

showElevator :: Elevator s n -> String
showElevator (Elevator d n) = "Elevator " <> showDoorState d <> " " <> showSNat n

elevatorDoorState :: Elevator s n -> SDoorState s
elevatorDoorState (Elevator d _) = d

elevatorFloor :: Elevator s n -> SNat n
elevatorFloor (Elevator _ n) = n

-- Elevator motion
moveUpOne :: Elevator 'DoorClosed n -> Elevator 'DoorClosed (Nat1 + n)
moveUpOne (Elevator d n) = Elevator d (SS n)

moveUpM :: SNat m -> Elevator 'DoorClosed n -> Elevator 'DoorClosed (m + n)
moveUpM SZ e = e
moveUpM (SS m_) e = moveUpOne $ moveUpM m_ e

moveDownOne :: Elevator 'DoorClosed (Nat1 + n) -> Elevator 'DoorClosed n
moveDownOne (Elevator d (SS n_)) = Elevator d n_

moveDownM :: IsLE m n -> Elevator 'DoorClosed n -> Elevator 'DoorClosed (n - m)
moveDownM (IsLE SZ _) e = e
moveDownM (IsLE (SS m_) (SS n_)) (Elevator d _) = moveDownM (IsLE m_ n_) (Elevator d n_)

-- Elevator motion commutative
moveUpOne' :: Elevator 'DoorClosed n -> Elevator 'DoorClosed (n + Nat1)
moveUpOne' (Elevator d n) =
  Elevator d (SS n)
    & gcastWith (sumIsCommutative n snat1)

moveUpM' :: SNat m -> Elevator 'DoorClosed n -> Elevator 'DoorClosed (n + m)
moveUpM' SZ e@(Elevator _ n) = gcastWith (sumIsCommutative SZ n) e
moveUpM' (SS m_) e@(Elevator _ n) =
  moveUpOne' (moveUpM' m_ e)
    & gcastWith (sumIsAssociative n m_ snat1)
    & gcastWith (sumIsCommutative m_ snat1)

-- Note: Is it possible to remove SingI n?
moveDownOne' :: forall n. (SingI n) => Elevator 'DoorClosed (n + Nat1) -> Elevator 'DoorClosed n
moveDownOne' (Elevator d m) =
  Elevator d (pred m)
    & gcastWith (sumIsCommutative (sing @n) snat1)

moveDownM' :: IsLE m n -> Elevator 'DoorClosed n -> Elevator 'DoorClosed (n - m)
moveDownM' (IsLE SZ _) e = e
moveDownM' (IsLE (SS m_) (SS n_)) (Elevator d _) = moveDownM' (IsLE m_ n_) (Elevator d n_)

elevatorSpec :: SpecWith ()
elevatorSpec = describe "Elevator" $ do
  let e0 = Elevator SDoorClosed snat0
      e1 = Elevator SDoorClosed snat1
      e2 = Elevator SDoorClosed snat2
      e3 = Elevator SDoorClosed snat3
      e4 = Elevator SDoorClosed snat4

  it "basic" $ do
    elevatorFloor e0 `shouldBe` snat0
    elevatorFloor e1 `shouldBe` snat1
    elevatorDoorState e0 `shouldBe` SDoorClosed
    elevatorDoorState e0 `shouldBe` SDoorClosed

  it "moveUpOne" $ do
    moveUpOne e0 `shouldBe` e1
    moveUpOne e1 `shouldBe` e2
    moveUpOne e2 `shouldBe` e3
    moveUpOne e0 `shouldBe` moveUpOne' e0
    moveUpOne e1 `shouldBe` moveUpOne' e1

  it "moveUp" $ do
    moveUpM snat0 e1 `shouldBe` e1
    moveUpM snat1 e2 `shouldBe` e3
    moveUpM snat1 e3 `shouldBe` e4
    moveUpM snat5 e1 `shouldBe` moveUpM' snat5 e1
    moveUpM snat0 e1 `shouldBe` moveUpM' snat0 e1
    moveUpM snat1 e2 `shouldBe` moveUpM' snat1 e2
    moveUpM snat1 e3 `shouldBe` moveUpM' snat1 e3

  it "moveDown" $ do
    moveDownOne e1 `shouldBe` moveDownOne' e1
    moveDownM' (IsLE snat3 snat5) (Elevator SDoorClosed snat5) `shouldBe` moveDownM (IsLE snat3 snat5) (Elevator SDoorClosed snat5)

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

zeroLEZero :: IsLE Nat0 Nat0
zeroLEZero = IsLE snat0 snat0

zeroLEOne :: IsLE Nat0 Nat1
zeroLEOne = isLEStep zeroLEZero

zeroLETwo :: IsLE Nat0 Nat2
zeroLETwo = isLEStep zeroLEOne

zeroLEThree :: IsLE Nat0 Nat3
zeroLEThree = isLEStep zeroLETwo

oneLEOne :: IsLE Nat1 Nat1
oneLEOne = isLESucc zeroLEZero

oneLETwo :: IsLE Nat1 Nat2
oneLETwo = isLEStep oneLEOne

oneLEThree :: IsLE Nat1 Nat3
oneLEThree = isLEStep oneLETwo

twoLETwo :: IsLE Nat2 Nat2
twoLETwo = isLESucc oneLEOne

twoLEThree :: IsLE Nat2 Nat3
twoLEThree = isLEStep twoLETwo

main :: IO ()
main = do
  traverse_
    (putStrLn . showSomeSNat)
    [ SomeNat snat0
    , SomeNat snat1
    , SomeNat snat2
    ]

  print $ showSNat snat10
  print $ showSNat snat20
  hspec spec

spec :: SpecWith ()
spec = describe "Nat" $ do
  let expectNatsEqual :: SNat n -> SNat m -> IO ()
      expectNatsEqual n m = case n %~ m of
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

  it "basic" $ do
    let test m n expectProved = expectProof (m %~ n) expectProved (show m <> "," <> show n)
    test snat0 snat0 True
    test snat0 snat1 False
    test snat0 snat2 False
    test snat1 snat0 False
    test snat1 snat1 True
    test snat1 snat2 False
    test snat2 snat0 False
    test snat2 snat1 False
    test snat2 snat2 True

  it "[toNatural]" $ do
    let test (SomeNat n) x = toNatural n `shouldBe` x
    traverse_
      (uncurry test)
      [ (SomeNat snat0, 0)
      , (SomeNat snat1, 1)
      , (SomeNat snat2, 2)
      , (SomeNat snat3, 3)
      , (SomeNat snat4, 4)
      , (SomeNat snat5, 5)
      , (SomeNat snat10, 10)
      , (SomeNat snat20, 20)
      ]

  it "[ %~]" $ do
    let test :: (SomeNat, SomeNat, Bool) -> IO ()
        test (SomeNat n, SomeNat m, expectProved) = expectProof (n %~ m) expectProved (show n <> "," <> show m)

    traverse_
      test
      [ (someNat 0, someNat 0, True)
      , (someNat 0, someNat 1, False)
      , (someNat 0, someNat 2, False)
      , (someNat 1, someNat 0, False)
      , (someNat 1, someNat 1, True)
      , (someNat 1, someNat 2, False)
      , (someNat 2, someNat 0, False)
      , (someNat 2, someNat 1, False)
      , (someNat 2, someNat 2, True)
      ]

    x <-
      traverse (\(n, m) -> test (someNat n, someNat m, m == n))
        $ (,)
        <$> [0 .. 20]
        <*> [0 .. 20]
    length x `shouldBe` 441

  describe "[+]" $ do
    it "basic" $ do
      traverse_
        (uncurry expectSomeNatsEqual)
        [ (SomeNat (snat0 %+ snat0), someNat 0)
        , (SomeNat (snat0 %+ snat1), someNat 1)
        , (SomeNat (snat0 %+ snat2), someNat 2)
        , (SomeNat (snat0 %+ snat3), someNat 3)
        , (SomeNat (snat1 %+ snat0), someNat 1)
        , (SomeNat (snat1 %+ snat1), someNat 2)
        , (SomeNat (snat1 %+ snat2), someNat 3)
        , (SomeNat (snat1 %+ snat3), someNat 4)
        , (SomeNat (snat2 %+ snat0), someNat 2)
        , (SomeNat (snat2 %+ snat1), someNat 3)
        , (SomeNat (snat2 %+ snat2), someNat 4)
        , (SomeNat (snat2 %+ snat3), someNat 5)
        ]

      let testSum (SomeNat n, SomeNat m, SomeNat x) = expectNatsEqual (n %+ m) x
      x <-
        traverse
          (\(n, m) -> testSum (someNat n, someNat m, someNat (n + m)))
          $ (,)
          <$> [0 .. 100]
          <*> [0 .. 100]

      length x `shouldBe` 10201
      void $ return x

    it "[sumIsCommutative]" $ do
      (sumIsCommutative' @Nat0 @Nat1) `shouldBe` Refl
      (sumIsCommutative' @Nat1 @Nat0) `shouldBe` Refl
      (sumIsCommutative' @Nat1 @Nat2) `shouldBe` Refl
      (sumIsCommutative' @Nat2 @Nat1) `shouldBe` Refl

    it "[sumIsCommutative] application" $ do
      x <-
        traverse
          ( \(SomeNat n, SomeNat m) -> do
              let x = n %+ m
              expectSomeNatsEqual (SomeNat x) (SomeNat (add2 n m))
              expectSomeNatsEqual (SomeNat x) (SomeNat (add2 m n))
          )
          $ (,)
          <$> take 30 allNats
          <*> take 100 allNats
      length x `shouldBe` 3000

    it "[sumIsAssociative]" $ do
      (sumIsAssociative' @Nat0 @Nat1 @Nat3) `shouldBe` Refl
      (sumIsAssociative' @Nat1 @Nat0 @Nat3) `shouldBe` Refl
      (sumIsAssociative' @Nat1 @Nat3 @Nat0) `shouldBe` Refl
      (sumIsAssociative' @Nat1 @Nat3 @Nat5) `shouldBe` Refl

    it "[sumIsAssociative] application" $ do
      x <-
        traverse
          ( \(SomeNat n, SomeNat m, SomeNat o) -> do
              let x = add3 n m o
              expectSomeNatsEqual (SomeNat ((n %+ m) %+ o)) (SomeNat x)
              expectSomeNatsEqual (SomeNat (n %+ (m %+ o))) (SomeNat x)
          )
          $ (,,)
          <$> take 20 allNats
          <*> take 20 allNats
          <*> take 20 allNats
      length x `shouldBe` 8000

  describe "[-]" $ do
    it "basic" $ do
      traverse_
        (uncurry testSomeNatsMaybe)
        [ (SomeNat <$> (snat0 %- snat0), Just (someNat 0))
        , (SomeNat <$> (snat0 %- snat1), Nothing)
        , (SomeNat <$> (snat0 %- snat2), Nothing)
        , (SomeNat <$> (snat0 %- snat3), Nothing)
        , (SomeNat <$> (snat1 %- snat0), Just (someNat 1))
        , (SomeNat <$> (snat1 %- snat1), Just (someNat 0))
        , (SomeNat <$> (snat1 %- snat2), Nothing)
        , (SomeNat <$> (snat1 %- snat3), Nothing)
        , (SomeNat <$> (snat2 %- snat0), Just (someNat 2))
        , (SomeNat <$> (snat2 %- snat1), Just (someNat 1))
        , (SomeNat <$> (snat2 %- snat2), Just (someNat 0))
        , (SomeNat <$> (snat2 %- snat3), Nothing)
        ]

  describe "IsLE" $ do
    it "[decideLE']" $ do
      let test (SomeNat m) (SomeNat n) = expectProof (decideLE m n) (toNatural m <= toNatural n) $ show m <> "<=" <> show n
      x <- traverse (uncurry test) $ (,) <$> take 20 allNats <*> take 20 allNats
      length x `shouldBe` 400

    it "[isLEStep]" $ do
      isLEStep zeroLEZero `shouldBe` zeroLEOne
      isLEStep zeroLEOne `shouldBe` zeroLETwo
      isLEStep oneLEOne `shouldBe` oneLETwo
      isLEStep oneLETwo `shouldBe` oneLEThree
      isLEStep twoLETwo `shouldBe` twoLEThree

    it "[isLEStepL]" $ do
      isLEStepL oneLEOne `shouldBe` zeroLEOne
      isLEStepL oneLETwo `shouldBe` zeroLETwo
      isLEStepL twoLETwo `shouldBe` oneLETwo
      isLEStepL twoLEThree `shouldBe` oneLEThree

    it "[isLEAsym]" $ do
      isLEAsym zeroLEZero zeroLEZero `shouldBe` Refl
      isLEAsym oneLEOne oneLEOne `shouldBe` Refl

    it "[isLETrans]" $ do
      isLETrans zeroLEZero zeroLEZero `shouldBe` zeroLEZero
      isLETrans zeroLEZero zeroLEOne `shouldBe` zeroLEOne
      isLETrans zeroLEZero zeroLETwo `shouldBe` zeroLETwo
      isLETrans zeroLEOne oneLEOne `shouldBe` zeroLEOne
      isLETrans zeroLEOne oneLETwo `shouldBe` zeroLETwo
      isLETrans zeroLETwo twoLETwo `shouldBe` zeroLETwo
      isLETrans oneLEOne oneLEOne `shouldBe` oneLEOne
      isLETrans oneLEOne oneLETwo `shouldBe` oneLETwo
      isLETrans oneLETwo twoLETwo `shouldBe` oneLETwo
      isLETrans oneLETwo twoLEThree `shouldBe` oneLEThree
      isLETrans twoLETwo twoLETwo `shouldBe` twoLETwo
      isLETrans twoLETwo twoLEThree `shouldBe` twoLEThree

  elevatorSpec
