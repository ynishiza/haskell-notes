#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

import Data.Kind
import Data.Typeable

main :: IO ()
main = do
  print $ typeOfSomeValue $ SomeValue $ ValueStr "a"
  print $ typeOfSomeValue $ SomeValue $ ValueInt 1

data Dict (c :: Constraint) where
  Dict :: (c) => Dict c

withDict :: Dict c -> ((c) => a) -> a
withDict Dict x = x

data Value a where
  ValueStr :: String -> Value String
  ValueInt :: Int -> Value Int

data SomeValue = forall a. SomeValue (Value a)

valueProperties :: Value a -> Dict (Typeable a)
valueProperties (ValueStr _) = Dict
valueProperties (ValueInt _) = Dict

typeOfSomeValue :: SomeValue -> TypeRep
-- typeOfSomeValue (SomeValue v) = case valueProperties v of Dict -> typeOf v
typeOfSomeValue (SomeValue v) = withDict (valueProperties v) $ typeOf v -- OK
-- typeOfSomeValue (Some1 v) = typeOf v -- No instance for (Typeable x) arising from a use of ‘typeOf’
