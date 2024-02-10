#!/usr/bin/env stack
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- | Module

Note: define a module to generate Haddock documentation per note
-}
module Note2024011Conduit where

import Conduit
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Data.Conduit.Combinators as CC hiding (print)
import Data.Conduit.List as CL
import Data.Function

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  result <-
    runConduit pipeline
      & runApp Env
  print result
  -- Stuff
  return ()

data Env = Env

runApp :: Env -> App a -> IO a
runApp env (App app) = runReaderT app env

newtype App a = App (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

data User = User String
  deriving (Show, Eq)
data Thing = Thing
data Result = Result
  deriving (Show, Eq)

pipeline :: ConduitT () Void App ()
pipeline =
  sourceUsers
    .| getThing
    .| computeResult
    .| consumeResult

getUserThing :: User -> App Thing
getUserThing _ = return Thing

sourceUsers :: ConduitT () User App ()
sourceUsers = CL.sourceList [User "A", User "B"]

getThing :: ConduitT User (User, Thing) App ()
getThing = CL.mapM $ \user -> do
  liftIO $ putStrLn $ "getThing: user" <> show user
  (user,) <$> getUserThing user

computeResult :: ConduitT (User, Thing) (User, Result) App ()
computeResult = mapC $ \(user, _) -> (user, Result)

consumeResult :: ConduitT (User, Result) Void App ()
consumeResult = CC.mapM_ $ \(user, result) -> do
  liftIO $ putStrLn $ "consumeResult user=" <> show user <> " result=" <> show result
