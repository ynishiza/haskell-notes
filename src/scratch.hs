#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

main :: IO ()
main = do
  pure ()
  -- putStrLn $ myShow (1 :: Int)
  -- putStrLn $ myShow True

  -- putStrLn $ myShow2 (1 :: Int)
  -- putStrLn $ myShow2 True
  -- print $ (doSomething @[Int]) [1]
  -- print $ (doSomething @(Maybe Int)) $ Just 1

class MyShow a where
  myShow :: a -> String

instance (Show a, Show b) => MyShow (Either a b) where

-- instance {-# OVERLAPPABLE #-} (Show a) => MyShow a where -- default
-- instance {-# OVERLAPS #-} (Show a) => MyShow a where -- default
--   myShow = show
-- instance MyShow Int where -- Int specific
--   myShow v = "INT " ++ show v
-- instance MyShow Bool where -- Bool specific
--   myShow v = "BOOL " ++ show v

-- class MyShow2 a where
--   myShow2 :: a -> String

-- instance (Show a) => MyShow2 a where -- default
--   myShow2 = show
-- instance {-# OVERLAPPING #-} MyShow2 Int where -- Int specific
--   myShow2 v = "INT " ++ show v
-- instance {-# OVERLAPS #-} MyShow2 Bool where -- Bool specific
--   myShow2 v = "BOOL " ++ show v


-- class MyS (a :: * -> *) where
-- instance MyS (Either a)
