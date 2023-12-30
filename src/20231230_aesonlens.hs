#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- Note: define a module to generate Haddock documentation per note
module Note20231230aesonLens where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Vector qualified as Vector
import Test.Hspec
import Utils

import Control.Lens
import Data.Maybe

main :: IO ()
main = hspec spec

expect :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
expect = flip shouldBe

hasKeyValue :: Key -> Value -> Value -> Bool
hasKeyValue k = hasValue (key k)

hasValue :: (Eq a) => Traversal' Value a -> a -> Value -> Bool
hasValue l v = (== Just v) . preview l

spec :: Spec
spec = describe "Main" $ do
  let
    decodeJson :: String -> Value
    decodeJson = fromJust . decode' . B.pack

  it "Value" $ do
    Null ^? _Null  & expect (Just ())
    String "" ^? _Null  & expect Nothing
    Number 1 ^? _Integer & expect (Just 1)
    Number 1 ^? _String & expect Nothing

  it "Array" $ do
    let source =
          decodeJson
            [multilineString|
  [
    { 
      "name": "A",
      "value": 1
    },
    {
      "name": "B",
      "value": 2
    }
  ]
|]
    let
      v1 =
        KeyMap.fromList
          [ ("name", String "A")
          , ("value", Number 1)
          ]
      v2 =
        KeyMap.fromList
          [ ("name", String "B")
          , ("value", Number 2)
          ]
      vect =
        Vector.fromList
          [ Object v1
          , Object v2
          ]

    source & expect (Array vect)
    source ^. _Array & expect vect
    source ^.. values & expect [Object v1, Object v2]
    source ^.. (_Array . each) & expect [Object v1, Object v2]
    source ^.. (values . key "name") & expect [String "A", String "B"]
    source ^.. (values . key "name" . _String) & expect ["A", "B"]
    source ^.. (values . key "name" . _Integer) & expect []

    -- filter
    source ^? (nth 1 . key "name") & expect (Just $ String "B")
    source ^? (values . filtered (hasValue (key "name" . _String) "A")) & expect (Just $ Object v1)
    source ^? (values . filtered (hasValue (key "value" . _Integer) 1)) & expect (Just $ Object v1)

    -- modify
    --
    -- append
    (_Array %~ (`Vector.snoc` String "hello")) source
      & expect
        ( Array
            $ Vector.fromList
              [ Object v1
              , Object v2
              , String "hello"
              ]
        )
    -- delete
    (_Array %~ Vector.filter (hasValue (key "name" . _String) "A")) source & expect (Array $ Vector.fromList [Object v1])
    ((values . filtered (hasValue (key "name" . _String) "A") . key "name" . _String) .~ "X") source
      & expect
        ( Array
            $ Vector.fromList
              [ Object $ KeyMap.insert "name" "X" v1
              , Object v2
              ]
        )
    ((nth 0 . key "name" . _String) .~ "X") source
      & expect
        ( Array
            $ Vector.fromList
              [ Object $ KeyMap.insert "name" "X" v1
              , Object v2
              ]
        )

  it "Object" $ do
    let source =
          decodeJson
            [multilineString|
    { 
      "name": "A",
      "value": 1
    }
|]

    source ^? key "name" & expect (Just $ String "A")
    source ^? _Object . ix "name" & expect (Just $ String "A")
    source ^? (key "name" . _String) & expect (Just "A")
    source ^? (key "name" . _Integer) & expect Nothing
    source ^? (atKey "name" . _Just) & expect (Just "A")
    source ^? (atKey "name" . _Just . _String) & expect (Just "A")
    source ^? (atKey "names" . _Nothing) & expect (Just ())

    -- update
    -- modify key
    -- (key "name" .~ String "a") source
    (key "name" . _String .~ "a") source
      & expect
        ( Object
            $ KeyMap.fromList
              [ ("name", String "a")
              , ("value", Number 1)
              ]
        )
    -- BAD. add new key
    (key "tag" .~ String "a") source
      & expect
        ( Object
            $ KeyMap.fromList
              [ ("name", String "A")
              , ("value", Number 1)
              ]
        )
    -- add new key
    -- (atKey "tag" .~ (Just $ String "a")) source
    (atKey "tag" ?~ Bool True) source
      & expect
        ( Object
            $ KeyMap.fromList
              [ ("name", String "A")
              , ("tag", Bool True)
              , ("value", Number 1)
              ]
        )
    -- remove key
    (atKey "name" .~ Nothing) source
      & expect
        ( Object
            $ KeyMap.fromList
              [ ("value", Number 1)
              ]
        )
