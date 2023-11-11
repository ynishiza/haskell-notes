#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# HLINT ignore "Use =<<" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Note20230911howDependentHaskellCanImproveIndustry where

import Data.Foldable
import Data.Kind (Type)
import Data.Singletons.TH (Sing, genSingletons)
import GHC.Natural

-- ======================================== PeanoNatural ========================================

data Peano = Z | S Peano

genSingletons [''Peano]

type PeanoNatural :: Peano -> Type
data PeanoNatural n = PeanoNatural !(Sing n) Natural

pnNParam :: PeanoNatural n -> Natural
pnNParam (PeanoNatural SZ _) = 0
pnNParam s@(PeanoNatural (SS _) _) = 1 + pnNParam (pnMinusOne s)

pnShow :: PeanoNatural n -> String
pnShow p@(PeanoNatural _ k) = "PeanoNatural @" <> show (pnNParam p) <> " " <> show k

pnMinusOne :: (n ~ 'S m) => PeanoNatural n -> PeanoNatural m
pnMinusOne (PeanoNatural (SS a) k) = PeanoNatural a (k - 1)

pnAddOne :: (n ~ 'S m) => PeanoNatural m -> PeanoNatural n
pnAddOne (PeanoNatural SZ _) = PeanoNatural (SS SZ) 1
pnAddOne (PeanoNatural s@(SS _) k) = PeanoNatural (SS s) $ k + 1

type family (+) (n :: Peano) (m :: Peano) where
  'Z + n = n
  ('S n) + m = 'S (n + m)

type family (-) (n :: Peano) (m :: Peano) :: Peano where
  n - 'Z = n
  ('S n) - ('S m) = n - m

(+:) :: PeanoNatural n -> PeanoNatural m -> PeanoNatural (n + m)
(PeanoNatural SZ _) +: x = x
(PeanoNatural (SS n) k) +: x =
  let (PeanoNatural n' k') = PeanoNatural n (k - 1) +: x
   in PeanoNatural (SS n') (k' + 1)

(-:) :: PeanoNatural n -> PeanoNatural m -> Maybe (PeanoNatural (n - m))
x -: (PeanoNatural SZ _) = Just x
(PeanoNatural (SS n) k) -: (PeanoNatural (SS m) l) = PeanoNatural n (k - 1) -: PeanoNatural m (l - 1)
(PeanoNatural SZ _) -: (PeanoNatural (SS _) _) = Nothing

pattern Zero :: (n ~ 'Z) => PeanoNatural n
pattern Zero = PeanoNatural SZ 0

pattern Succ :: (n ~ 'S m) => PeanoNatural m -> PeanoNatural n
pattern Succ s <- (pnValidate . pnMinusOne -> PNProof s)
  where
    Succ (PeanoNatural s k) = PeanoNatural (SS s) (k + 1)

data PNProof n where
  PNProof :: PeanoNatural n -> PNProof n
  PNDisproof :: PNProof n

pnValidate :: PeanoNatural n -> PNProof n
pnValidate n@(PeanoNatural SZ 0) = PNProof n
pnValidate s@(PeanoNatural (SS n) k) = case pnValidate (PeanoNatural n (k - 1)) of
  PNProof _ -> PNProof s
  PNDisproof -> PNDisproof
pnValidate _ = PNDisproof

isValid :: PNProof n -> Bool
isValid (PNProof _) = True
isValid PNDisproof = False

-- ======================================== SomePeanoNatural ========================================

data SomePeanoNatural = forall (n :: Peano). SomePeanoNatural (PeanoNatural n)

(+|) :: SomePeanoNatural -> SomePeanoNatural -> SomePeanoNatural
SomePeanoNatural x +| SomePeanoNatural y = SomePeanoNatural (x +: y)

(-|) :: SomePeanoNatural -> SomePeanoNatural -> Maybe SomePeanoNatural
SomePeanoNatural x -| SomePeanoNatural y = maybe Nothing (Just . SomePeanoNatural) (x -: y)

-- ======================================== Main ========================================

zero :: PeanoNatural 'Z
zero = Zero

one :: PeanoNatural ('S 'Z)
one = Succ zero

two :: PeanoNatural ('S ('S 'Z))
two = Succ one

three :: PeanoNatural ('S ('S ('S 'Z)))
three = Succ two

banner :: String -> String
banner label = y <> " " <> label <> " " <> y
 where
  y = "****************************************"

main :: IO ()
main = do
  let testPN :: String -> SomePeanoNatural -> IO ()
      testPN name (SomePeanoNatural x) = putStrLn $ name <> " : " <> pnShow x <> " \t\t isValid : " <> show (isValid $ pnValidate x)
      allNumbers =
        [ ("zero", SomePeanoNatural zero)
        , ("one", SomePeanoNatural one)
        , ("two", SomePeanoNatural two)
        , ("three", SomePeanoNatural three)
        ]
      testPNMaybe name (Just x) = testPN name x
      testPNMaybe name Nothing = putStrLn $ "NO VALUE:" <> name

  putStrLn $ banner "Validation"
  traverse_
    (uncurry testPN)
    $ allNumbers
    <> [ ("bad zero", SomePeanoNatural (PeanoNatural SZ 11))
       , ("bad one", SomePeanoNatural (PeanoNatural (SS SZ) 11))
       ]

  putStrLn $ banner "-|"
  traverse_ (uncurry testPNMaybe) $ (\(n, x) (m, y) -> (n <> " - " <> m, x -| y)) <$> allNumbers <*> allNumbers

  putStrLn $ banner "+|"
  traverse_ (uncurry testPN) $ (\(n, x) (m, y) -> (n <> " + " <> m, x +| y)) <$> allNumbers <*> allNumbers
