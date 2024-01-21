#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sum" #-}

-- | Ref: http://h2.jaguarpaw.co.uk/posts/foldl-traverses-state-foldr-traverses-anything/
module NoteFoldrVsFoldl where

import Criterion
import Criterion.Main
import Data.Foldable
import GHC.Magic

main :: IO ()
main = do
  let nums :: [Int]
      -- nums = [0 .. 10 ^ (6 :: Int)]
      nums = [0 .. 10 ^ (2 :: Int)]
      smallNums :: [Int]
      smallNums = [0 .. 20]

  putStrLn $ "foldl:" <> show (foldl (flip (:)) [] smallNums)
  putStrLn $ "foldlFromFoldrNaive:" <> show (foldlFromFoldrNaive (flip (:)) [] smallNums)
  putStrLn $ "foldr:" <> show (foldr (:) [] smallNums)
  putStrLn $ "foldrFromFoldl:" <> show (foldrFromFoldl (:) [] smallNums)

  -- Infinite list
  putStrLn $ "foldr: foldr (:) [] [1..]" <> show (head $ foldr (:) [] [1 :: Int ..])
  -- putStrLn $ "foldrFromFoldl: foldr (:) [] [1..]" <> show (head $ foldrFromFoldl (:) [] [1 :: Int ..])       doesn't work?

  defaultMain
    [ bench "foldlFromFoldrMagic" (whnf (foldlFromFoldrMagic (+) 0) nums)
    , bench "foldlFromFoldrNaive" (whnf (foldlFromFoldrNaive (+) 0) nums)
    , -- , bench "foldr" (whnf (foldr (+) 0) nums)
      -- , bench "foldl" (whnf (foldl (+) 0) nums)
      -- , bench "foldr'" (whnf (foldr' (+) 0) nums)
      bench "foldl'" (whnf (foldl' (+) 0) nums)
    ]
  pure ()

-- |
-- Does not handle infinite lists?
foldrFromFoldl :: forall a b t. (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldrFromFoldl f z0 xs = foldl g id xs z0
 where
  g :: (b -> b) -> a -> (b -> b)
  g k x z = k $ f x z

foldlFromFoldrNaive :: forall a b t. (Foldable t) => (b -> a -> b) -> b -> t a -> b
foldlFromFoldrNaive f z0 xs = foldr g id xs z0
 where
  g :: a -> (b -> b) -> (b -> b)
  g x k z = seq z (k (f z x))

{- | Same implementation as base library
https://hackage.haskell.org/package/base-4.16.3.0/docs/src/GHC.List.html#foldl%27
Still not very fast?

@

foldl'           :: forall a b . (b -> a -> b) -> b -> [a] -> b
{\-# INLINE foldl' #-\}
foldl' k z0 xs =
   foldr (\(v::a) (fn::b->b) -> oneShot (\(z::b) -> z `seq` fn (k z v))) (id :: b -> b) xs z0
@
-}
foldlFromFoldrMagic :: forall a b t. (Foldable t) => (b -> a -> b) -> b -> t a -> b
{-# INLINE foldlFromFoldrMagic #-}
foldlFromFoldrMagic f z0 xs = foldr (\(x :: a) k -> oneShot (\(z :: b) -> seq z (k (f z x)))) (id :: b -> b) xs z0
