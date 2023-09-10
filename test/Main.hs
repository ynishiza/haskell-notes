{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import B.Lib qualified
import Test.Hspec
import Utils

main :: IO ()
main = hspec $ describe "App" $ do
  it "Hello" $ do
    putStrLn basePath
    B.Lib.message `shouldBe` "Hello"
