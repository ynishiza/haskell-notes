#!/usr/bin/env stack
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Note20230918patternSynonym (pattern Empty) where

import Data.Kind (Type)

main :: IO ()
main = do
  let l0 = Empty :: List Int
      l1 = Succ 1 $ Succ 1 l0
  print l1
  printNum 2
  printNum 3
  pure ()

data List a = ListEmpty | ListSucc a (List a) Int
  deriving (Show, Eq)

pattern Empty :: List a
pattern Empty = ListEmpty

pattern Succ :: a -> List a -> List a
pattern Succ a list <- ListSucc a list _
  where
    Succ a l
      | ListSucc _ _ n <- l = ListSucc a l (n + 1)
      | ListEmpty <- l = ListSucc a ListEmpty 1

{-# COMPLETE Empty, Succ #-}

listLength :: List a -> Int
listLength Empty = 0
listLength (ListSucc _ _ n) = n

empty :: List a
empty = ListEmpty

succ :: a -> List a -> List a
succ a ListEmpty = ListSucc a ListEmpty 1
succ a l@(ListSucc _ _ n) = ListSucc a l (n + 1)

pattern Even :: (Integral a) => a -> a
pattern Even n <- (\x -> if even x then Just x else Nothing -> Just n)

pattern Odd :: (Integral a) => a -> a
pattern Odd n <- (\x -> if odd x then Just x else Nothing -> Just n)

{-# COMPLETE Even, Odd #-}

printNum :: (Show a, Integral a) => a -> IO ()
printNum (Even n) = putStrLn $ "Even:" <> show n
printNum (Odd n) = putStrLn $ "Odd:" <> show n

-- pattern A a <- Just a
pattern A :: (Ord a) => a -> a -> [a]
pattern A a b <- [a, b]
  where
    A a b
      | a > b = [a]
      | otherwise = [b]

-- pattern B (a, a) <- Just a
--

data FileType = Txt | Log

type SFileType :: FileType -> Type
data SFileType filetype where
  STxt :: SFileType 'Txt
  SLog :: SFileType 'Log

type File :: FileType -> Type
data File filetype = File (SFileType filetype) FilePath

-- pattern TxtFile :: FilePath -> File 'Txt       -- BAD. Cannot pattern match any File
pattern TxtFile :: -- GOOD
  forall (filetype :: FileType).
  () => -- match any File
  (filetype ~ 'Txt) => -- construct File 'Txt
  FilePath ->
  File filetype
pattern TxtFile p = File STxt p

-- pattern LogFile :: FilePath -> File 'Txt       -- BAD. Cannot pattern match any File
pattern LogFile :: -- GOOD
  forall (filetype :: FileType).
  () => -- match any File
  (filetype ~ 'Log) => -- construct File 'Log
  FilePath ->
  File filetype
pattern LogFile p = File SLog p

{-# COMPLETE TxtFile, LogFile #-}

instance Show (File filetype) where
  show (TxtFile path) = "TxtFile " <> path
  show (LogFile path) = "LogFile " <> path

textFile :: File 'Txt
textFile = TxtFile "a.txt"

logFile :: File 'Log
logFile = LogFile "a.log"
