#!/usr/bin/env stack
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}

import Control.Applicative
import Control.Arrow (first)
import Control.Exception (Exception)
import Control.Monad as X
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State qualified as S
import Control.Monad.Writer
import Data.Data (Typeable)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack

-- ============================== Start Base ==============================

data CParserError state = HasCallStack => CParserError state Text

deriving stock instance Show state => Show (CParserError state)

deriving anyclass instance (Show state, Typeable state) => Exception (CParserError state)

type OnSuccess state result m a = (a, state) -> m result

type OnFail error result m = error -> m result

-- | CPS style parser
newtype CParserT error state result m a = CParserT
  { runCParserBase :: state -> OnSuccess state result m a -> OnFail error result m -> m result
  }

instance Functor (CParserT error state result m) where
  fmap f (CParserT p) = CParserT $ \state k ke -> p state (k . first f) ke

instance Applicative (CParserT error state result m) where
  pure x = CParserT $ \state k _ -> k (x, state)
  CParserT fp <*> CParserT xp = CParserT $ \state k ke -> fp state (\(f, state') -> xp state' (k . first f) ke) ke

instance Alternative (CParserT (CParserError state) state result m) where
  empty = CParserT $ \state _ ke -> ke (CParserError state "empty")
  CParserT xp <|> CParserT yp = CParserT $ \state k ke -> xp state k (\_ -> yp state k ke)

instance MonadPlus (CParserT (CParserError state) state result m)

instance Monad (CParserT error state result m) where
  CParserT xp >>= h = CParserT $ \state k ke -> xp state (\(x, state') -> (runCParserBase $ h x) state' k ke) ke

instance MonadFail (CParserT (CParserError state) state result m) where
  fail msg = CParserT $ \ust _ ke -> ke (CParserError ust (T.pack msg))

execCParser :: Monad m => CParserT error state (Either error (a, state)) m a -> state -> m (Either error (a, state))
execCParser (CParserT f) i = f i (return . Right) (return . Left)

instance MonadTrans (CParserT error state result) where
  lift m = CParserT $ \state k _ -> m >>= k . (,state)

instance MonadBase b m => MonadBase b (CParserT error state result m) where
  liftBase x = lift $ liftBase x

instance MonadReader r m => MonadReader r (CParserT error state result m) where
  ask = lift ask
  local f (CParserT p) = CParserT $ \state k ke -> local f (p state k ke)

instance S.MonadState r m => S.MonadState r (CParserT error state result m) where
  get = lift S.get
  put x = lift $ S.put x

instance MonadWriter w m => MonadWriter w (CParserT error state result m) where
  tell x = lift $ tell x
  pass (CParserT p) = CParserT $ \state k ke -> p state (\(a, state') -> pass (pure a) >>= k . (,state')) ke
  listen (CParserT p) = CParserT $ \state k ke ->
    p
      state
      ( \(a, state') -> listen (pure a) >>= k . (,state')
      )
      ke

-- ============================== End Base ==============================

type ParserState = (Int, String)

type Parser result = CParserT (CParserError ParserState) ParserState result Identity

satisfy :: HasCallStack => Text -> (Char -> Bool) -> Parser result Char
satisfy name f = CParserT g
  where
    g s@(i, c : rest) k ke = if f c then k (c, (i + 1, rest)) else ke (CParserError s ("[satisfy] Failed to parse " <> name))
    g s@(_, []) _ ke = ke $ CParserError s "[satisfy] Empty input"

char :: HasCallStack => Char -> Parser result Char
char c = satisfy (T.pack [c]) (== c)

string :: HasCallStack => String -> Parser result String
string = traverse char

main :: IO ()
main = do
  let test p text = putStrLn $ case runIdentity (execCParser p text) of
        Left e@(CParserError _ _) -> show e <> "\n" <> prettyCallStack callStack
        Right v -> show v

  let p1 = some $ char 'a'
  test p1 (0, "aaaabb")
  test p1 (0, "bbcc")

  let p2 = many (char 'a' <|> char 'b')
  test p2 (0, "aabbcc")
  test p2 (0, "xxxx")

  let p3 = (<>) <$> string "hello" <*> string "world"
  test p3 (0, "helloworld abc")
  test p3 (0, "abc")
  return ()
