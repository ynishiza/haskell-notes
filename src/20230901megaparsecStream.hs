#!/usr/bin/env stack
{-
  Run with
   stack ghci -- src/scratch/ScratchUtils.hs src/scratch/megaparsecStream.hs
   stack exec -- src/scratch/ScratchUtils.hs src/scratch/megaparsecStream.hs
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Function
import Data.Functor
import Data.List.Extra
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Void
import Debug.Trace (trace)
import Utils
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  parseTest (withTrace abcdeString) [A, B, C, D, E, F]
  parseTest abcde [A, B, C, D, E, F]
  parseTest (count 3 abError >> single Newline >> abError) [A, B, A, B, A, B, Newline, A, B]
  parseTest hello [Hello]

  hspec spec

enableDebug :: Bool
enableDebug = True

trace_ :: String -> a -> a
trace_ = if enableDebug then trace else (\_ x -> x)

withTrace :: Parser a -> Parser a
withTrace p = traceState *> p <* traceState

traceState :: Parser ()
traceState = do
  state <- getParserState
  trace_ (show state) $ return ()

type Parser = Parsec Void [Term]

data Term = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Hello | Newline
  deriving (Show, Eq, Ord)

instance VisualStream [Term] where
  showTokens _ = showTokens_ . NE.toList
  tokensLength _ cs =
    NE.toList cs
      <&> length . showTerm
      & sum

instance TraversableStream [Term] where
  reachOffset
    offset
    p@( PosState
          { pstateOffset,
            pstateInput,
            pstateSourcePos =
              SourcePos
                { sourceName,
                  sourceLine
                },
            pstateLinePrefix
          }
        ) =
      trace_
        debugInfo
        ( Just (fullCurrentLinePrefix <> showTokens_ restOfCurrentLine),
          newPosState
        )
      where
        -- Step 1: determine consumed and unconsumed tokens in current state
        (consumed, unconsumed) = splitAt (offset - pstateOffset) pstateInput

        -- Step 2: determine if current state is the same line as the previous state
        -- If it is, then need to prepend tokens that were consumed by the previous state
        -- in order to determine the position in the current line.
        consumedLines = split (== Newline) consumed
        isSameLine = length consumedLines == 1
        (currentLinePrefix, consumedInCurrentLine) =
          if isSameLine
            then -- case: if same line, continue from previous state
              (pstateLinePrefix, consumed)
            else -- case: if new line, ignore previous state
              ("", last consumedLines)

        -- Step 3: compute all tokens in the current line
        fullCurrentLinePrefix = currentLinePrefix <> showTokens_ consumedInCurrentLine
        restOfCurrentLine = takeWhile (/= Newline) unconsumed

        newPos =
          SourcePos
            { sourceName = sourceName,
              sourceLine = changePos (+ (length consumedLines - 1)) sourceLine,
              sourceColumn = mkPos (length fullCurrentLinePrefix + 1)
            }
        newPosState =
          PosState
            { pstateOffset = offset,
              pstateInput = unconsumed,
              pstateSourcePos = newPos,
              pstateTabWidth = mkPos 1,
              pstateLinePrefix = fullCurrentLinePrefix
            }

        debugInfo =
          "(offset, initial state):\t"
            <> show (offset, p)
            <> "\n"
            <> "new state::\t"
            <> show newPosState
            <> "\n"
            <> "(isSameLine, consumed, consumedInCurrentLine, restOfCurrentLine):\t"
            <> show (isSameLine, consumed, consumedInCurrentLine, restOfCurrentLine)
            <> "line"
            <> (fullCurrentLinePrefix <> showTokens_ restOfCurrentLine)
            <> "\n"

changePos :: (Int -> Int) -> Pos -> Pos
changePos f = mkPos . f . unPos

showTerm :: Term -> String
showTerm Newline = "\n"
showTerm c = show c

showTokens_ :: [Term] -> String
showTokens_ = intercalate "" . (showTerm <$>)

onLine :: Parser a -> Parser a
onLine = (<* single Newline)

abcde :: Parser [Term]
abcde = single A >> single B >> single C >> single D >> single E >> return [A, B, C, D, E]

abcdeString :: Parser [Term]
abcdeString = string [A, B, C, D, E]

abError :: Parser [Term]
abError = do
  o <- getOffset
  registerParseError (FancyError o $ S.singleton $ ErrorFail "FAKE ERROR")
  string [A, B]

hello :: Parser Term
hello = single Hello

trimSpaces :: String -> String
trimSpaces = dropWhile f . dropWhileEnd f
  where
    f = flip elem " \n"

replicate_ :: Int -> [a] -> [a]
replicate_ n = concat . replicate n

spec :: Spec
spec = describe "stream" $ do
  let testError :: Parser a -> [Term] -> String -> Expectation
      testError p input msg = do
        case runParser p "" input of
          Left e -> trimSpaces (errorBundlePretty e) `shouldBe` trimSpaces msg
          Right _ -> expectationFailure $ "Parser did not fail with input " <> show input

  it "[Token]" $ do
    -- error at beginning
    testError
      abcde
      [U, A, B, C, D, E]
      [multilineString|
1:1:
  |
1 | UABCDE
  | ^
unexpected U
expecting A
      |]

    -- error in middle
    testError
      abcde
      [A, B, F, D, E]
      [multilineString|
1:3:
  |
1 | ABFDE
  |   ^
unexpected F
expecting C
      |]

    -- error at end
    testError
      (count 2 abcde)
      [A, B, C, D, E, A, B, F]
      [multilineString|
1:8:
  |
1 | ABCDEABF
  |        ^
unexpected F
expecting C
          |]

  it "[Tokens]" $ do
    -- error at beginning
    testError
      abcdeString
      [U, A, B, C, D, E]
      [multilineString|
1:1:
  |
1 | UABCDE
  | ^^^^^
unexpected UABCD
expecting ABCDE
        |]

    -- error in middle
    testError
      abcdeString
      [A, B, F, D, E]
      [multilineString|
1:1:
  |
1 | ABFDE
  | ^^^^^
unexpected ABFDE
expecting ABCDE
        |]

    testError
      (count 2 abcdeString)
      [A, B, C, D, E, A, B, F]
      [multilineString|
1:6:
  |
1 | ABCDEABF
  |      ^^^
unexpected ABF
expecting ABCDE
        |]

  it "[Token] different line" $ do
    testError
      (count 10 (onLine abcde))
      (replicate_ 3 [A, B, C, D, E, Newline] <> [A, B, F, D])
      [multilineString|
4:3:
  |
4 | ABFD
  |   ^
unexpected F
expecting C
|]

  it "[Tokens] different line" $ do
    testError
      (count 10 (onLine abcdeString))
      (replicate_ 3 [A, B, C, D, E, Newline] <> [A, B, F, D])
      [multilineString|
4:1:
  |
4 | ABFD
  | ^^^^
unexpected ABFD
expecting ABCDE
|]

  it "multiple errors" $ do
    testError
      (onLine (count 3 abError) >> abError)
      [A, B, A, B, A, B, Newline, A, B]
      [multilineString|
1:1:
  |
1 | ABABAB
  | ^
FAKE ERROR

1:3:
  |
1 | ABABAB
  |   ^
FAKE ERROR

1:5:
  |
1 | ABABAB
  |     ^
FAKE ERROR

2:1:
  |
2 | AB
  | ^
FAKE ERROR
|]
