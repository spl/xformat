{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.XFormat.Read
-- Copyright   :  (c) Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines a simple, extensible, type-indexed function for reading
-- well-typed values from a string with a format descriptor. This may be
-- considered a Haskell variant of the C @scanf@ function.
--
-- If you are primarily interested in using this library, you will want to see
-- 'readsf' and 'readf', the more user-friendly functions.
--
-- If you are also interested in extending this library with your own format
-- descriptors, you should read about the 'Format' class.
--------------------------------------------------------------------------------

module Text.XFormat.Read (

  -- * The Class

  Format(..),

  -- * The Functions

  readsf,
  readf,

  -- * Format Descriptors

  -- | These are used to indicate which values and types to read.

  (:%:)(..),
  (%),
  CharF(..),
  IntF(..),
  IntegerF(..),
  FloatF(..),
  DoubleF(..),
  StringF(..),
  ReadF(..),
  NumF(..),
  SpaceF(..),
  WrapF(..),

) where

--------------------------------------------------------------------------------

import Text.XFormat.Common
import Text.ParserCombinators.ReadP
import Data.Char (isSpace)

--------------------------------------------------------------------------------

-- | This class provides the signature for an extensible, type-indexed function
-- that uses a format descriptor to parse a string input and return a well-typed
-- value. The type variable @d@ is the format descriptor, and the variable @a@
-- is the type of the value to be read from the input.
--
-- An instance of @Format@ adds a (type) case to the function. Before defining
-- an instance, you must first define a format descriptor for your specific type
-- and expected input. The descriptor is often very simple. See the descriptors
-- in this module for examples.
--
-- Here is the instance for types that are instances of 'Read'.
--
-- > data ReadF a = Read -- Format descriptor
-- >
-- > instance (Read a) => Format (ReadF a) a where
-- >   readpf Read = readS_to_P reads
--
-- Note that you will need some of the combinators (such as 'readS_to_P') in
-- "Text.ParserCombinators.ReadP".

class Format d a | d -> a where

  -- | Given a format descriptor @d@, return a 'ReadP' parser for a type @a@.
  -- This function may not be very useful outside of defining an instance for
  -- 'Format'. Instead, consider using 'readsf' or 'readf'.

  readpf :: d -> ReadP a

--------------------------------------------------------------------------------

-- | Given a format descriptor @d@ and a 'String', return a list of successes
-- for the type @a@, i.e. @[(a, 'String')]@. This function simply transforms the
-- 'ReadP' parser of 'readpf' to a 'ReadS' function.

readsf :: (Format d a) => d -> ReadS a
readsf fmt = readP_to_S (readpf fmt)

-- | Given a format descriptor @d@ and a 'String', return an optional result of
-- the type @a@. This function simply returns the head of the list from 'readsf'
-- if it was successful.

readf :: (Format d a) => d -> String -> Maybe a
readf fmt s = headfirst (readsf fmt s)
  where
    headfirst []        = Nothing
    headfirst ((a,_):_) = Just a

--------------------------------------------------------------------------------

--
-- Format constants
--
-- These are not descriptors in the traditional sense. These are constants that
-- are compared with parsed input.
--

-- | Parse a 'String' in the input and return it if it is equal to the enclosed
-- string.

instance Format String String where
  readpf = string

-- | Parse a 'Char' in the input and return the string if it is equal to the
-- enclosed character.

instance Format Char Char where
  readpf = char

--------------------------------------------------------------------------------

--
-- Basic format descriptors
--

-- | Parse a 'Char' in the input.

instance Format CharF Char where
  readpf Char = get

-- | Parse a 'String' in the input. Reads until end of input.

instance Format StringF String where
  readpf String = munch (const True)

-- | Parse an 'Int' in the input.

instance Format IntF Int where
  readpf Int = readS_to_P reads

-- | Parse an 'Integer' in the input.

instance Format IntegerF Integer where
  readpf Integer = readS_to_P reads

-- | Parse a 'Float' in the input.

instance Format FloatF Float where
  readpf Float = readS_to_P reads

-- | Parse a 'Double' in the input.

instance Format DoubleF Double where
  readpf Double = readS_to_P reads

--------------------------------------------------------------------------------

--
-- Class format descriptors
--

-- | Parse a @'Read' a@ value in the input.

instance (Read a) => Format (ReadF a) a where
  readpf Read = readS_to_P reads

-- | Parse a @'Num' a@ value in the input.

instance (Read a, Num a) => Format (NumF a) a where
  readpf Num = readS_to_P reads

--------------------------------------------------------------------------------

--
-- Other format descriptors
--

-- | Parse a @'ReadP' a@ value in the input.

instance Format (ReadP a) a where
  readpf = id

-- | Parse all whitespace in the input.

instance Format SpaceF String where
  readpf Space = munch isSpace

instance (Format d1 a1, Format d2 a2)
  => Format (WrapF d1 d2) (a1 :%: a2 :%: a1) where
  readpf (Wrap d1l d2 d1r) = do
    a1 <- readpf d1l
    a2 <- readpf d2
    a3 <- readpf d1r
    return (a1 :%: a2 :%: a3)

--------------------------------------------------------------------------------

--
-- Recursive format descriptors
--

-- | Right-associative product: First parse an @a1@ value and then an @a2@
-- value, returning the pair as @a1 :%: a2@.

instance (Format d1 a1, Format d2 a2) => Format (d1 :%: d2) (a1 :%: a2) where
  readpf (d1 :%: d2) = do
    a1 <- readpf d1
    a2 <- readpf d2
    return (a1 :%: a2)

testTwo = readsf (Int % "abc") "5abcd" == [(5 % "abc", "d")]
testThree = readsf (Char % 'a' % Read) "5a'b'" == [('5' % 'a' % 'b', "")]

--------------------------------------------------------------------------------

--
-- Tuple format descriptors: These all follow the same pattern.
--

instance (Format d1 a1, Format d2 a2) => Format (d1, d2) (a1, a2) where
  readpf (d1, d2) = do
    a1 <- readpf d1
    a2 <- readpf d2
    return (a1, a2)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3)
  => Format
  (d1, d2, d3)
  (a1, a2, a3)
  where
  readpf (d1, d2, d3) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    return (a1, a2, a3)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4)
  => Format
  (d1, d2, d3, d4)
  (a1, a2, a3, a4)
  where
  readpf (d1, d2, d3, d4) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    return (a1, a2, a3, a4)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5)
  => Format
  (d1, d2, d3, d4, d5)
  (a1, a2, a3, a4, a5)
  where
  readpf (d1, d2, d3, d4, d5) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    return (a1, a2, a3, a4, a5)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6)
  => Format
  (d1, d2, d3, d4, d5, d6)
  (a1, a2, a3, a4, a5, a6)
  where
  readpf (d1, d2, d3, d4, d5, d6) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    return (a1, a2, a3, a4, a5, a6)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7)
  => Format
  (d1, d2, d3, d4, d5, d6, d7)
  (a1, a2, a3, a4, a5, a6, a7)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    return (a1, a2, a3, a4, a5, a6, a7)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8)
  (a1, a2, a3, a4, a5, a6, a7, a8)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    return (a1, a2, a3, a4, a5, a6, a7, a8)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9, Format d10 a10)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    a10 <- readpf d10
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9, Format d10 a10,
   Format d11 a11)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    a10 <- readpf d10
    a11 <- readpf d11
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9, Format d10 a10,
   Format d11 a11, Format d12 a12)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    a10 <- readpf d10
    a11 <- readpf d11
    a12 <- readpf d12
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9, Format d10 a10,
   Format d11 a11, Format d12 a12, Format d13 a13)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    a10 <- readpf d10
    a11 <- readpf d11
    a12 <- readpf d12
    a13 <- readpf d13
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9, Format d10 a10,
   Format d11 a11, Format d12 a12, Format d13 a13, Format d14 a14)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    a10 <- readpf d10
    a11 <- readpf d11
    a12 <- readpf d12
    a13 <- readpf d13
    a14 <- readpf d14
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

instance
  (Format d1 a1, Format d2 a2, Format d3 a3, Format d4 a4, Format d5 a5,
   Format d6 a6, Format d7 a7, Format d8 a8, Format d9 a9, Format d10 a10,
   Format d11 a11, Format d12 a12, Format d13 a13, Format d14 a14,
   Format d15 a15)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15)
  (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
  where
  readpf (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15) = do
    a1 <- readpf d1
    a2 <- readpf d2
    a3 <- readpf d3
    a4 <- readpf d4
    a5 <- readpf d5
    a6 <- readpf d6
    a7 <- readpf d7
    a8 <- readpf d8
    a9 <- readpf d9
    a10 <- readpf d10
    a11 <- readpf d11
    a12 <- readpf d12
    a13 <- readpf d13
    a14 <- readpf d14
    a15 <- readpf d15
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

