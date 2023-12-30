#!/usr/bin/env stack
{-# LANGUAGE FlexibleInstances #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- Note: define a module to generate Haddock documentation per note
module Note20231105dict where

import Data.Kind
import Data.Typeable

main :: IO ()
main = do
  print $ typeOfSomeValue $ SomeValue $ ValueStr "a"
  print $ typeOfSomeValue $ SomeValue $ ValueInt 1
  print $ doSomethingWithSomeValue $ SomeValue $ ValueStr "a"
  print $ doSomethingWithSomeValue $ SomeValue $ ValueInt 1

data Dict (c :: Constraint) where
  Dict :: (c) => Dict c

withDict :: Dict c -> ((c) => a) -> a
withDict Dict x = x

data Value a where
  ValueStr :: String -> Value String
  ValueInt :: Int -> Value Int

data SomeValue = forall a. SomeValue (Value a)

instance Show SomeValue where
  show (SomeValue (ValueInt x)) = show x
  show (SomeValue (ValueStr x)) = x

class DoSomething x where
  doSomething :: x -> x

instance DoSomething (Value String) where doSomething (ValueStr x) = ValueStr $ x <> x
instance DoSomething (Value Int) where doSomething (ValueInt x) = ValueInt $ 2 * x

valueProperties :: Value a -> Dict (Typeable a, DoSomething (Value a))
valueProperties (ValueStr _) = Dict
valueProperties (ValueInt _) = Dict

doSomethingWithSomeValue :: SomeValue -> SomeValue
doSomethingWithSomeValue (SomeValue v) = withDict (valueProperties v) $ SomeValue $ doSomething v
-- doSomethingWithSomeValue (SomeValue v) = case (valueProperties v) of Dict -> SomeValue $ doSomething v
-- doSomethingWithSomeValue (SomeValue v) = SomeValue $ doSomething v

typeOfSomeValue :: SomeValue -> TypeRep
-- typeOfSomeValue (SomeValue v) = case valueProperties v of Dict -> typeOf v
typeOfSomeValue (SomeValue v) = withDict (valueProperties v) $ typeOf v -- OK
-- typeOfSomeValue (Some1 v) = typeOf v -- No instance for (Typeable x) arising from a use of ‘typeOf’
