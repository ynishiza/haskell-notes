{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
-- Note: define a module to generate Haddock documentation per note
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE  TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Scratch where

import Data.Singletons
import Data.Singletons.TH
import Control.Lens hiding (Index)
import Data.Kind (Constraint, Type)
import Data.Map

main :: IO ()
main = do
  print $ getValue 1 "abc"
  print $ getValue 10 "abc"
  print $ setValue 0 'x' "abc"
  print $ setValue 1 'x' "abc"

  print $ setValue 0 False (True, True)
  print $ setValue 1 False (True, True)

  pure ()

type MyCollection :: Type -> Constraint
class MyCollection c where
  type MyKey c :: Type
  type MyValue c :: Type
  valuePath :: MyKey c -> Lens' c (Maybe (MyValue c))

instance MyCollection [a] where
  type MyKey [a] = Int
  type MyValue [a] = a
  valuePath key f = go key
   where
    go n l
      | n == 0, (a : xs) <- l = maybe l (: xs) <$> f (Just a)
      | n > 0, (a : xs) <- l = (a :) <$> go (n - 1) xs
      | otherwise = l <$ f Nothing

instance MyCollection (a, a) where
  type MyKey (a, a) = Int
  type MyValue (a, a) = a
  valuePath 0 f l@(x, y) = maybe l (,y) <$> f (Just x)
  valuePath 1 f l@(x, y) = maybe l (x,) <$> f (Just y)
  valuePath _ f l = l <$ f Nothing

getValue :: (MyCollection c) => MyKey c -> c -> Maybe (MyValue c)
getValue key = view (valuePath key)

setValue :: (MyCollection c) => MyKey c -> MyValue c -> c -> c
setValue key value = set (valuePath key) (Just value)

type Index :: Type -> Type
type family Index c

type Value :: Type -> Type
type family Value c

type instance Index [a] = Int
type instance Index (a, a) = Int
type instance Index (Map k a) = k

type instance Value [a] = a
type instance Value (Map k a) = a

genSingletons [''Maybe, ''Bool]

type SMaybe2 ::forall x. Maybe x -> Type
data SMaybe2 k where
  SNothing2 :: SMaybe2 'Nothing
  SJust2 :: Sing x -> SMaybe2 ('Just x)
