-- run with
--
--   $ stack ghci -- src/scratch/genericsExample.hs
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import Data.Typeable
import GHC.Generics

class ConstructorInfo m where
  getConstructorName :: m a -> String

instance ConstructorInfo a => ConstructorInfo (M1 D (s :: Meta) a) where
  getConstructorName (M1 x) = getConstructorName x

instance Constructor s => ConstructorInfo (M1 C (s :: Meta) a) where
  getConstructorName = conName

instance (ConstructorInfo f, ConstructorInfo g) => ConstructorInfo (f :+: g) where
  getConstructorName (L1 x) = getConstructorName x
  getConstructorName (R1 x) = getConstructorName x

class FieldInfo m where
  getFieldInfo :: m a -> [(String, TypeRep)]

instance {-# OVERLAPPING #-} (Selector s, Typeable a) => FieldInfo (M1 S (s :: Meta) (K1 R a)) where
  getFieldInfo x@(M1 (K1 v)) = [(selName x, typeOf v)]

instance FieldInfo f => FieldInfo (M1 m (s :: Meta) f) where
  getFieldInfo (M1 x) = getFieldInfo x

instance (FieldInfo f, FieldInfo g) => FieldInfo (f :+: g) where
  getFieldInfo (L1 x) = getFieldInfo x
  getFieldInfo (R1 x) = getFieldInfo x

instance (FieldInfo f, FieldInfo g) => FieldInfo (f :*: g) where
  getFieldInfo (x :*: y) = getFieldInfo x ++ getFieldInfo y

data MyData = MyData {name :: String, value :: Int}
  deriving stock (Show, Eq, Generic)

data Alpha = A | B | C | D
  deriving stock (Show, Eq, Generic)

myDataFields :: [(String, TypeRep)]
myDataFields = getFieldInfo $ from $ MyData "a" 1

test :: IO ()
test = do
  let value = MyData "a" 1

  {-
  MyData fields:[("name",[Char]),("value",Int)]
  MyData name:"MyData"
  MyData constructor:"MyData"
    -}
  putStrLn $ "MyData fields:" <> show (getFieldInfo $ from value)
  putStrLn $ "MyData name:" <> show (datatypeName $ from value)
  putStrLn $ "MyData constructor:" <> show (getConstructorName $ from value)

  {-
  A name:"Alpha"
  B name:"Alpha"
  A constructor:"A"
  B constructor:"B"
    -}
  putStrLn $ "A name:" <> show (datatypeName $ from A)
  putStrLn $ "B name:" <> show (datatypeName $ from B)
  putStrLn $ "A constructor:" <> show (getConstructorName $ from A)
  putStrLn $ "B constructor:" <> show (getConstructorName $ from B)
