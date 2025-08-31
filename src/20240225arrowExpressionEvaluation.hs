#!/usr/bin/env stack
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- | Module

Ref: https://www.haskell.org/arrows/biblio.html
Paper: "Generalizing Monads to Arrows"

Section 4.2 "Arrows and interpreters"
-}
module NoteExpressionEvaluationWithArrows (
  main,

  -- * Common
  ExpError (..),
  Expression (..),
  ArrowMonadTransform (..),

  -- * Monadic evaluation
  ValueM (..),
  eval,

  -- * Arrow evaluation
  KleisliIO,
  ArrowConditions,
  runEvalA,
  ValueA (..),
  evalA,
) where

import Control.Arrow
import Control.Category
import Control.Exception.Safe
import Data.Function hiding (id, (.))
import Data.Functor
import Data.Kind (Type)
import GHC.Stack
import Prelude hiding (exp, id, pred, (.))

-- * main

--

{- $info
Infos
-}

main :: IO ()
main = do
  -- Stuff
  runEvalA (evalA sample1) sampleEnv >>= putStrLn . (("result:" <>) . show)
  runEvalA (evalA sample1) emptyEnv >>= putStrLn . (("result:" <>) . show)
  return ()

emptyEnv :: EnvA (Kleisli IO)
emptyEnv = Env []

sampleEnv :: EnvA (Kleisli IO)
sampleEnv =
  Env
    [ ("b", BoolA False)
    , ("x", NumA 1)
    , ("y", NumA 2)
    ]

sample1 :: Expression
sample1 =
  If
    (Var "b")
    (Add (Var "x") (Var "y"))
    (Var "x")

-- * Common

--

-- ** Error
data ExpError = (HasCallStack) => ExpError String

instance Exception ExpError

instance Show ExpError where
  show (ExpError msg) = "ExpErrror " <> msg <> "\n" <> prettyCallStack callStack

instance Eq ExpError where ExpError m1 == ExpError m2 = m1 == m2

-- ** Environment

-- | Variable table [(name, value)]
type Env :: Type -> Type
newtype Env value = Env [(String, value)]

type Expression :: Type
data Expression
  = Var String
  | Add Expression Expression
  | -- |
    -- @If predicate ifValue elseValue@
    If Expression Expression Expression
  | -- |
    --  @Lambda variable expression@
    Lambda String Expression
  | -- |
    -- @App lambda input@
    App Expression Expression

type KleisliIO = Kleisli IO

class (Arrow (arrow m)) => ArrowMonadTransform arrow m where
  arrM :: (a -> m b) -> arrow m a b

instance (Monad m) => ArrowMonadTransform Kleisli m where
  arrM = Kleisli

-- ==================== Monadic ====================

type ValueM :: (Type -> Type) -> Type
data ValueM m
  = NumM Int
  | BoolM Bool
  | FunM (ValueM m -> m (ValueM m))

instance Show (ValueM arrow) where
  show (NumM x) = show x
  show (BoolM x) = show x
  show (FunM _) = "Fun"

-- | Variable table [(name, value)]
type EnvM m = Env (ValueM m)

eval :: (MonadCatch m, HasCallStack) => Expression -> EnvM m -> m (ValueM m)
eval (Var var) (Env env) = case lookup var env of
  Just val -> return val
  Nothing -> throwM $ ExpError ""
eval (Add expr1 expr2) env = do
  value1 <- eval expr1 env
  value2 <- eval expr2 env
  case (value1, value2) of
    (NumM n1, NumM n2) -> return $ NumM $ n1 + n2
    _ -> throwM $ ExpError ""
eval (If predicate expr1 expr2) env = do
  v1 <- eval predicate env
  case v1 of
    BoolM b -> if b then eval expr1 env else eval expr2 env
    _ -> throwM $ ExpError ""
eval (Lambda var exp) (Env env) = return $ FunM $ \v -> eval exp (Env $ (var, v) : env)
eval (App func expr2) env = do
  v1 <- eval func env
  case v1 of
    FunM f -> eval expr2 env >>= f
    _ -> throwM $ ExpError ""

-- ==================== Arrow ====================
--

runEvalA :: KleisliIO (EnvA KleisliIO) (ValueA KleisliIO) -> EnvA KleisliIO -> IO (Maybe (ValueA KleisliIO))
runEvalA arrow env = catch (Just <$> result) onError
 where
  result = runKleisli arrow env
  onError e@(ExpError _) = print e $> Nothing

type ValueA :: (Type -> Type -> Type) -> Type
data ValueA arrow
  = NumA Int
  | BoolA Bool
  | FunA (arrow (ValueA arrow) (ValueA arrow))

instance Show (ValueA arrow) where
  show (NumA x) = show x
  show (BoolA x) = show x
  show (FunA _) = "Fun"

{- | Variable table [(name, value)]
type EnvA :: (Type -> Type -> Type) -> Type
newtype EnvA arrow = EnvA [(String, ValueA arrow)]
-}
type EnvA arrow = Env (ValueA arrow)

createFunA :: (ArrowConditions arrow m) => String -> Expression -> EnvA (arrow m) -> ValueA (arrow m)
createFunA var expr (Env env) =
  FunA $ arr (\v -> Env ((var, v) : env)) >>> evalA expr

type ArrowConditions arrow m =
  ( ArrowChoice (arrow m)
  , ArrowApply (arrow m)
  , ArrowMonadTransform arrow m
  , MonadCatch m
  , HasCallStack
  )

numA :: (ArrowConditions arrow m) => arrow m (ValueA (arrow m)) Int
numA = numA' >>> throwA (("Not a number:" <>) . show) ||| id

throwA :: (ArrowConditions arrow m) => (a -> String) -> arrow m a b
throwA msg = arrM (throwM . ExpError . msg)

numA' :: (ArrowConditions arrow m) => arrow m (ValueA (arrow m)) (Either (ValueA (arrow m)) Int)
numA' = arr $ \case NumA n -> Right n; v -> Left v

boolA' :: (ArrowConditions arrow m) => arrow m (ValueA (arrow m)) (Either (ValueA (arrow m)) Bool)
boolA' = arr $ \case BoolA b -> Right b; v -> Left v

boolA :: (ArrowConditions arrow m) => arrow m (ValueA (arrow m)) Bool
boolA = boolA' >>> throwA (("Not a bool:" <>) . show) ||| id

funA' :: (ArrowConditions arrow m) => arrow m (ValueA (arrow m)) (Either (ValueA (arrow m)) (arrow m (ValueA (arrow m)) (ValueA (arrow m))))
funA' = arr $ \case FunA f -> Right f; v -> Left v

funA :: (ArrowConditions arrow m) => arrow m (ValueA (arrow m)) (arrow m (ValueA (arrow m)) (ValueA (arrow m)))
funA = funA' >>> throwA (("Not a function:" <>) . show) ||| id

evalA :: (ArrowConditions arrow m) => Expression -> arrow m (EnvA (arrow m)) (ValueA (arrow m))
evalA (Var name) = arrM $ \(Env env) -> case lookup name env of
  Just value -> return value
  Nothing -> throwM $ ExpError $ "No variable " <> name
evalA (Add expr1 expr2) =
  evalA expr1
    &&& evalA expr2
    >>> (numA *** numA)
    >>> arr (\(n1, n2) -> NumA (n1 + n2))
evalA (If predicate expr1 expr2) =
  (evalA predicate >>> boolA)
    &&& id
    >>> arr (\(pred, env) -> if pred then Right env else Left env)
    >>> (evalA expr1 ||| evalA expr2)
evalA (Lambda var expr) = id >>> arr (createFunA var expr)
evalA (App fexp e) =
  (evalA fexp >>> funA)
    &&& evalA e
    >>> app
