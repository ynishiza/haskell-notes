#!/usr/bin/env stack

-- Note: define a module to generate Haddock documentation per note
module Note20230901stream where

import Data.Function
import Streaming
import Streaming.Prelude qualified as S

stream1 :: Stream (Of Int) IO ()
stream1 =
  S.yield 1
    <> S.yield 2
    <> S.yield 3
    <> S.yield 4

stream2 :: Stream (Of Int) IO ()
stream2 =
  return ()
    & wrap
    . (1 :>)
    & wrap
    . (2 :>)
    & wrap
    . (3 :>)

main :: IO ()
main = do
  stream1
    & S.map (* 3)
    & S.filter (> 4)
    & S.print

  stream2
    & S.print
