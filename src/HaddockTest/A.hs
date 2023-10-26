{- |
 Module: HaddockTest.A
 Description: Test module
 Copyright: (c) 2023
 License: GPL-3
 Maintainer: yui@test.com
 Stability: 
 Portability: POSIX

 Build this with

 > haddock -o temp --html --hyperlinked-source src/HaddockTest/*.hs
-}
module HaddockTest.A
  ( -- * Usage
    --
    -- $usage

    -- ** Section A
    -- 
    -- | Section A info 
    --
    -- blah blah
    a,

    -- ** Section B
    --
    -- | Section B info 
    --
    -- blah blah
    b,
    f,

    -- * Test
    --
    -- $test
    --
    MyData(..)
  )
where

-- | Some value a
a :: Int
a = 10

-- | Some value b
b :: Int
b = 10

-- | `f` is a function
--
-- abc
f :: a -> a
f = id

-- $usage
--
-- Usage info
--
-- blah blah


-- $test
--
-- Test ground
--
-- /italic/
-- __bold__
-- @monospace@
--
-- Bulleted list
--
--  * item 1
--  This is item 1
--
--  * item 2
--
--  Numbered list
--
--   1. item1
--   2. item2
--
--
-- >>> 1 + 1 = 2
-- False
--
-- > 1 + 1 = 2
-- > False
--
-- >>> a
-- b
--
-- > f x = 1
-- > g x = 1
--
-- @1+1=2@ a
--
-- @
-- 1+1=2
-- 2+3=5
-- @
--
--
-- Type 'MyData'
--
-- Type v'MyData'
--
-- Type t'MyData'
--

data MyData = MyData
