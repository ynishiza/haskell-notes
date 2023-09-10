-- Run with 
--  stack exec -- src/scratch/<name>.hs
--  stack ghci -- src/scratch/<name>.hs
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import Data.Attoparsec.ByteString.Char8
import Control.Applicative

word :: Parser String
word = many1 (satisfy (`elem` ['0' .. 'z'])) <?> "word"

whitespace :: Parser String
whitespace = many1 space <?> "spaces"

wordsParser :: Parser [String]
wordsParser =
  ( optional whitespace
      >> sepBy1 word whitespace
        <* ((whitespace >> endOfInput) <|> endOfInput)
  )
    <?> "words"
