#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Ref: https://www.haskell.org/arrows/biblio.html
Paper: "Generalizing Monads to Arrows"

Section 6 "Stream Processors"
-}
module NoteTemplate (
  main,
  spec,
  SP (..),
  get,
  put,
) where

import Control.Arrow
import Control.Category
import Data.Function hiding (id, (.))
import Test.Hspec
import Prelude hiding (id, (.))

-- import Prelude qualified

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  print $ take 3 $ fst $ readOne integersInfinite ()
  readOne (first integersInfinite) ((), ())
    & snd
    & flip readOne ((), ())
    -- & snd
    -- & flip readOne ((), ())
    & fst
    & print
  hspec spec
  readOne (integersInfinite &&& integersInfinite) ()
    & snd
    & flip readOne ()
    & snd
    & flip readOne ()
    & fst
    & print
  return ()

spec :: SpecWith ()
spec = describe "" $ do
  let
    readAndTake :: SP a b -> a -> [b]
    readAndTake sp v = fst $ readOne sp v

    readNTimes :: SP a b -> a -> Int -> [[b]]
    readNTimes sp v n = fst $ readN sp (replicate n v)

  it "integersList" $ do
    (readAndTake integersInfinite () & take 3)
      `shouldBe` [0, 1, 2]

  it "[first]" $ do
    readNTimes (first integersInfinite) ((), ()) 5
      `shouldBe` [ []
                 , [(0, ())]
                 , [(1, ())]
                 , [(2, ())]
                 , [(3, ())]
                 ]

  it "[<+>] merge" $ do
    readNTimes (intCounter <+> (intCounter >>> addSP 3)) () 5
      `shouldBe` [ [0, 3]
                 , [1, 4]
                 , [2, 5]
                 , [3, 6]
                 , [4, 7]
                 ]

  it "[&&&] parallel" $ do
    readNTimes (intCounter &&& (intCounter >>> showSP)) ((), ()) 5
      `shouldBe` [ []
                 , [(0, "0")]
                 , [(1, "1")]
                 , [(2, "2")]
                 , [(3, "3")]
                 ]

  it "[|&|] multiplex" $ do
    readNTimes (intCounter |&| (intCounter >>> multN 2 >>> showSP)) () 5
      `shouldBe` [ [Left 0, Right "0"]
                 , [Left 1, Right "2"]
                 , [Left 2, Right "4"]
                 , [Left 3, Right "6"]
                 , [Left 4, Right "8"]
                 ]

  it "[dyn] " $ do
    fst
      ( readN
          intCounterWithReset
          [ False
          , False
          , False
          , False
          , True
          , True
          , False
          , False
          ]
      )
      `shouldBe` [ [0]
                 , [1]
                 , [2]
                 , [3]
                 , [4]
                 , [0]
                 , [0]
                 , [1]
                 ]

integersInfinite :: SP a Int
integersInfinite = f 0
 where
  f n = put n (f (n + 1))

intCounter :: SP a Int
intCounter = put 0 (f 1)
 where
  -- Wait for input
  f n = get $ const $ put n (f (n + 1))

multN :: (Num a) => a -> SP a a
multN n = arr (* n)

addSP :: (Num a) => a -> SP a a
addSP n = arr (+ n)

showSP :: (Show a) => SP a String
showSP = arr show

intCounterWithReset :: SP Bool Int
intCounterWithReset = intCounterWithReset' >>> dyn

intCounterWithReset' :: SP Bool (Either (SP () Int) ())
intCounterWithReset' = Put (Left intCounter) f
 where
  f :: SP Bool (Either (SP () Int) ())
  f = Get $ \case
    -- case: continue same counter
    False -> (Put (Right ()) f)
    -- case: reset
    True -> (Put (Left intCounter) f)

-- ============================== SP ==============================

-- | Process input @a@ and output @b@
data SP a b
  = -- | Receive input @a@
    Get (a -> SP a b)
  | -- | Output @a@
    Put b (SP a b)

put :: b -> SP a b -> SP a b
put = Put

get :: (a -> SP a b) -> SP a b
get = Get

readOne :: SP a b -> a -> ([b], SP a b)
readOne (Put b sp) a = let (output, sp') = readOne sp a in (b : output, sp')
readOne (Get f) a = ([], f a)

readN :: SP a b -> [a] -> ([[b]], SP a b)
readN sp [] = ([], sp)
readN sp (a : as) = let (b, sp') = readOne sp a in first (b :) (readN sp' as)

instance Category SP where
  id = Get $ \x -> Put x id

  (.) :: SP b c -> SP a b -> SP a c
  -- note: order matters
  -- Step 1: First flush all pending outputs i.e. handle the Puts
  Put c spb . spa = Put c (spb . spa)
  Get f . Put b sp = f b . sp
  -- Step 2: Request for input when there is nothing left to flush.
  sp . Get fa = Get $ \x -> sp . fa x

instance Arrow SP where
  arr :: (a -> b) -> SP a b
  arr f = Get $ \x -> Put (f x) (arr f)

  first :: SP a b -> SP (a, d) (b, d)
  first sp = zipValue sp []

-- | zips @d@ to @SP a b@ to create @SP (a, d) (b, d)@
zipValue :: SP a b -> [d] -> SP (a, d) (b, d)
zipValue (Put b sp) (d : ds) = Put (b, d) (zipValue sp ds)
zipValue (Put b sp) [] = Get
  $
  -- for getting d only, so ignore the first argument
  \(_, d) -> Put (b, d) (zipValue sp [])
zipValue (Get f) ds = Get $ \(a, d) -> zipValue (f a) (ds ++ [d])

{- | Parallel streams

Two completely parallel streams @f :: SP a b@ and @g :: SP a' b'@.

> f &&& g :: SP (Either a a') (Either b b')

Same input stream split in two outputs @f :: SP a b@ and @g :: SP a b'@.

> f |&| g :: SP a (Either b b')
-}
instance ArrowChoice SP where
  -- note: implement +++ instead of first so that the left stream is processed first
  -- By default right is processed first
  -- i.e. by default
  -- @
  --   Put a spa +++ Put b spb = Put b (Put a (spa +++ spb))
  -- @
  (+++) :: SP a b -> SP a' b' -> SP (Either a a') (Either b b')
  (Put b sp) +++ g = Put (Left b) (sp +++ g)
  f +++ (Put c sp) = Put (Right c) (f +++ sp)
  Get f1 +++ Get f2 = Get $ \case
    Left x -> f1 x +++ Get f2
    Right x -> Get f1 +++ f2 x

-- | Stream with no result
instance ArrowZero SP where
  zeroArrow = Get (const zeroArrow)

{- | Merges two streams into one.

> f <+> g :: SP a b

 Note:

- output of @f@ is flushed first
- checks for input only when there is no more to flush
-}
instance ArrowPlus SP where
  -- Step 1: flush first stream
  Put a sp1 <+> sp2 = Put a (sp1 <+> sp2)
  -- Step 2: flush second stream
  sp1 <+> Put a sp2 = Put a (sp1 <+> sp2)
  -- Step 3: wait for input when there is nothing left to flush
  Get f <+> Get f2 = Get $ \a -> f a <+> f2 a

-- | A stream interleaving two streams @SP a b@ and @SP a c@
(|&|) :: SP a b -> SP a c -> SP a (Either b c)
f |&| g = (arr Left <+> arr Right) >>> (f +++ g)

dyn :: SP (Either (SP a b) a) b
dyn = withStream zeroArrow
 where
  withStream :: SP a b -> SP (Either (SP a b) a) b
  withStream (Put x sp) = Put x (withStream sp)
  withStream (Get f) = Get $ \case
    -- case: continue using same stream
    Right a -> withStream (f a)
    -- case: switch to inner stream
    Left sp -> withStream sp
