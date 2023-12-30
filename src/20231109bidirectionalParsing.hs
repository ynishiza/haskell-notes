#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs

  Bidirectional parsing: https://kowainik.github.io/posts/haskell-mini-patterns#bidirectional-parsing
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Note: define a module to generate Haddock documentation per note
module Note20231109bidirectionalParsing where

main :: IO ()
main = putStrLn "Hello"

data Alpha = A | B | C
  deriving (Eq, Bounded, Enum, Show)

inverseMap :: forall e s. (Eq s, Enum e, Bounded e) => (e -> s) -> s -> Maybe e
inverseMap f input = foldr checkResult Nothing allValues
 where
  checkResult _ x@(Just _) = x
  checkResult value Nothing = if f value == input then Just value else Nothing
  allValues = enumFromTo (minBound @e) (maxBound @e)

toString :: Alpha -> String
toString A = "a"
toString B = "b"
toString C = "c"

fromString :: String -> Maybe Alpha
fromString = inverseMap toString 

fromString_ :: String -> Maybe Alpha
fromString_ "a" = Just A
fromString_ "B" = Just B
fromString_ "C" = Just C
fromString_ _ = Nothing
