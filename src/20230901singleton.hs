#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PolyKinds #-}

-- Note: define a module to generate Haddock documentation per note
module Note20230901singleton where

import Data.ByteString qualified as B
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH
import Data.Text qualified as T
import Data.Text.Encoding
import GHC.Types (Symbol)

data FileEncoding = Latin1 | UTF8 | UTF16 | UTF32

genSingletons [''FileEncoding, ''Bool]

type DataFile :: FileEncoding -> Type
data DataFile a where
  DataFile :: SingI a => FilePath -> DataFile a

readDataFile :: forall a. DataFile a -> IO T.Text
readDataFile (DataFile path) = withSing @a $ \case
    SLatin1 -> decodeLatin1 <$> B.readFile path
    SUTF8 -> decodeUtf8 <$> B.readFile path
    SUTF16 -> decodeUtf16BE <$> B.readFile path
    SUTF32 -> decodeUtf32BE <$> B.readFile path

message :: forall (a :: Bool). SingI a => Proxy a -> String
message _ = withSing @a $ \case
  STrue -> "A" 
  SFalse -> "B"

messageBad :: forall (a :: Bool). Proxy a -> String
messageBad _ = undefined 

type family Message (a :: Bool) :: Symbol where
  Message 'True = "A"
  Message 'False = "B"

main :: IO ()
main = do
  putStrLn $ message $ Proxy @'True
  putStrLn $ message $ Proxy @'False
  print $ fromSing STrue

