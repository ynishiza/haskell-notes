#!/usr/bin/env stack
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-
  Run with
    stack exec -- src/20260630_strictness_analysis.hs
    stack ghci -- src/scratch/<name>.hs

    $ stack ghc -- -O2 src/20260630_strictness_analysis.hs  -ddump-stranal -fforce-recomp

      [1 of 2] Compiling Main             ( src/20260630_strictness_analysis.hs, src/20260630_strictness_analysis.o ) [Source file changed]
      [2 of 2] Linking src/20260630_strictness_analysis [Objects changed]
      ld: warning: -U option is redundant when using -undefined dynamic_lookup
      500000.5
           112,100,920 bytes allocated in the heap
             1,498,344 bytes copied during GC
                44,328 bytes maximum residency (2 sample(s))
                33,496 bytes maximum slop
                     6 MiB total memory in use (0 MiB lost due to fragmentation)

                                           Tot time (elapsed)  Avg pause  Max pause
        Gen  0        25 colls,     0 par    0.002s   0.002s     0.0001s    0.0022s
        Gen  1         2 colls,     0 par    0.000s   0.002s     0.0012s    0.0022s

        INIT    time    0.006s  (  0.006s elapsed)
        MUT     time    0.017s  (  0.016s elapsed)
        GC      time    0.002s  (  0.005s elapsed)
        EXIT    time    0.000s  (  0.010s elapsed)
        Total   time    0.025s  (  0.037s elapsed)

        %GC     time       0.0%  (0.0% elapsed)

        Alloc rate    6,479,447,430 bytes per MUT second

        Productivity  69.6% of total user, 44.6% of total elapsed

    $ stack ghc -- -O2 src/20260630_strictness_analysis.hs  &&./src/20260630_strictness_analysis +RTS -s
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module

Note: define a module to generate Haddock documentation per note
Note: Use export list since LSP rename feature isn't supported without it.
-}
module Main (
  -- * main

  --
  main,
) where

data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }

-- case: least optimal
printListAverageUnoptimized :: [Int] -> IO ()
printListAverageUnoptimized = go (RunningTotal 0 0)
 where
  go rt [] = printAverage rt
  -- Bang on x is enough to make it strict
  go (RunningTotal sum count) (x : xs) =
    go (RunningTotal (sum + x) (count + 1)) xs

-- case: Snoyman's optimization
-- Add bangs in go.
printListAverageSnoyman :: [Int] -> IO ()
printListAverageSnoyman = go (RunningTotal 0 0)
 where
  go rt [] = printAverage rt
  -- Bangs here
  go (RunningTotal !sum !count) (x : xs) =
    go (RunningTotal (sum + x) (count + 1)) xs

-- case: S. Graf's optimization
-- Add bang in the print
printAverageBest :: RunningTotal -> IO ()
printAverageBest (RunningTotal !sum count) -- New bang here
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- case: bang in $!
printAverageBest2 :: RunningTotal -> IO ()
printAverageBest2 (RunningTotal sum count) -- New bang here
  | count == 0 = error "Need at least one value!"
  | otherwise = print $! (fromIntegral sum / fromIntegral count :: Double)

printListAverageBest :: [Int] -> IO ()
printListAverageBest = go (RunningTotal 0 0)
 where
  -- go rt [] = printAverageBest2 rt
  go rt [] = printAverageBest rt
  go (RunningTotal sum count) (x : xs) =
    -- No more bangs here
    go (RunningTotal (sum + x) (count + 1)) xs

-- case: Yui's optimization
-- Add bang on !x
printListAverageSimple :: [Int] -> IO ()
printListAverageSimple = go (RunningTotal 0 0)
 where
  go rt [] = printAverage rt
  -- Bang on x is enough to make it strict
  go (RunningTotal sum count) (!x : xs) =
    go (RunningTotal (sum + x) (count + 1)) xs

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  -- \| count == 0 = undefined
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- case: use Fold to remove list fusion
-- Most efficient
printListAverageFold :: [Int] -> IO ()
printListAverageFold = printAverage . foldl f (RunningTotal 0 0)
 where
  f (RunningTotal sum count) x = RunningTotal (sum + x) (count + 1)

-- case: No IO
printListAverageValue :: [Int] -> Double
printListAverageValue = go (RunningTotal 0 0)
 where
  go rt [] = printAverageValue rt
  go (RunningTotal sum count) (x : xs) =
    go (RunningTotal (sum + x) (count + 1)) xs

printAverageValue :: RunningTotal -> Double
printAverageValue (RunningTotal sum count)
  -- \| count == 0 = undefined
  | count == 0 = error "Need at least one value!"
  | otherwise = fromIntegral sum / fromIntegral count :: Double

fact :: Int -> Int -> Int
fact r 0 = r
fact r n = fact (n * r) (n - 1)
fac :: Int -> Int
fac = fact 1

fact' :: Int -> Int -> Int
fact' !r 0 = r
fact' !r !n = fact' (n * r) (n - 1)
fac' :: Int -> Int
fac' = fact' 1

main :: IO ()
main = do
  -- printListAverageUnoptimized [1..1000000]
  -- printListAverageSimple [1..1000000]
  -- printListAverageFold [1 .. 1000000]
  -- printListAverageBest [1..1000000]
  -- print $ printListAverageValue [1 .. 1000000]
  --
  -- print $ fac 1000000000
  print $ fac' 10000
