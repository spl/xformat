{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.XFormat.Read
-- Copyright   :  (c) 2009-2012 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines an extensible, type-indexed function for reading
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

  -- ** Basic Format Descriptors

  CharF(..),
  IntF(..),
  IntegerF(..),
  FloatF(..),
  DoubleF(..),
  StringF(..),
  RestF(..),

  -- ** Class-based Format Descriptors

  ReadF(..),
  NumF(..),

  -- ** Recursive Format Descriptors

  (:%:)(..),
  (%),

  WrapF(..),
  MaybeF(..),
  ChoiceF(..),
  EitherF(..),
  EitherLF(..),

  -- ** Other Format Descriptors

  SpaceF(..),

) where

--------------------------------------------------------------------------------

import Control.Applicative ((<$>))
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
-- Here is the instance for types that are instances of 'Prelude.Read'.
--
-- @
--   data 'ReadF' a = 'Read' -- Format descriptor
-- @
--
-- @
--   instance ('Prelude.Read' a) => Format ('ReadF' a) a where
--     'readpf' 'Read' = 'readS_to_P' 'reads'
-- @
--
-- Note that you will need some of the combinators (such as 'readS_to_P') in
-- "Text.ParserCombinators.ReadP".

class Format f where
  type R f :: *

  -- | Given a format descriptor @d@, return a 'ReadP' parser for a type @a@.
  -- This function may not be very useful outside of defining an instance for
  -- 'Format'. Instead, consider using 'readsf' or 'readf'.

  readpf :: f -> ReadP (R f)

--------------------------------------------------------------------------------

-- | Given a format descriptor @d@ and a 'String', return a list of successes
-- for the type @a@, i.e. @[(a, 'String')]@. This function simply transforms the
-- 'ReadP' parser of 'readpf' to a 'ReadS' function.

readsf :: Format f => f -> ReadS (R f)
readsf fmt = readP_to_S (readpf fmt)

-- | Given a format descriptor @d@ and a 'String', return an optional result of
-- the type @a@. This function simply returns the head of the list from 'readsf'
-- if it was successful.

readf :: Format f => f -> String -> Maybe (R f)
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

-- | Parse a 'String' and return it if it is equal to the enclosed value.

instance Format String where
  type R String = String
  readpf = string

-- | Parse a 'Char' and return it if it is equal to the enclosed value.

instance Format Char where
  type R Char = Char
  readpf = char

--------------------------------------------------------------------------------

--
-- Basic format descriptors
--

-- | Parse a character.

data CharF = Char

instance Format CharF where
  type R CharF = Char
  readpf Char = get

-- | Parse a non-empty string.

data StringF = String

instance Format StringF where
  type R StringF = String
  readpf String = many1 (satisfy (const True))

-- | Parse a possibly empty string greedily. This is useful for capturing the
-- rest of the input.

data RestF = Rest

instance Format RestF where
  type R RestF = String
  readpf Rest = munch (const True)

-- | Parse an 'Int'.

data IntF = Int

instance Format IntF where
  type R IntF = Int
  readpf Int = readS_to_P reads

-- | Parse an 'Integer'.

data IntegerF = Integer

instance Format IntegerF where
  type R IntegerF = Integer
  readpf Integer = readS_to_P reads

-- | Parse a 'Float'.

data FloatF = Float

instance Format FloatF where
  type R FloatF = Float
  readpf Float = readS_to_P reads

-- | Parse a 'Double'.

data DoubleF = Double

instance Format DoubleF where
  type R DoubleF = Double
  readpf Double = readS_to_P reads

--------------------------------------------------------------------------------

--
-- Class format descriptors
--

-- | Parse a value whose type is an instance of the class 'Prelude.Read'.

data ReadF a = Read

instance Read a => Format (ReadF a) where
  type R (ReadF a) = a
  readpf Read = readS_to_P reads

-- | Parse a value whose type is an instance of the class 'Prelude.Num'.

data NumF a = Num

instance (Read a, Num a) => Format (NumF a) where
  type R (NumF a) = a
  readpf Num = readS_to_P reads

--------------------------------------------------------------------------------

--
-- Other format descriptors
--

-- | Parse a @'ReadP' a@ value.

instance Format (ReadP a) where
  type R (ReadP a) = a
  readpf = id

-- | Parse zero or more whitespace characters. Stop when a non-whitespace
-- character is reached.

data SpaceF = Space

instance Format SpaceF where
  type R SpaceF = String
  readpf Space = munch isSpace

--------------------------------------------------------------------------------

--
-- Recursive format descriptors
--

-- | Right-associative pair. First parse a @a@-type format and then a @b@-type
-- format.

data a :%: b = a :%: b
  deriving (Eq, Show)

infixr 8 :%:

-- | Right-associative pair. This is a shorter, functional equivalent to the
-- type @(:%:)@.

(%) :: a -> b -> a :%: b
(%) = (:%:)

infixr 8 %

instance (Format f1, Format f2) => Format (f1 :%: f2) where
  type R (f1 :%: f2) = R f1 :%: R f2
  readpf (f1 :%: f2) = do
    a1 <- readpf f1
    a2 <- readpf f2
    return (a1 :%: a2)

-- | Parse a format of one type wrapped by two other formats of a different
-- type.

data WrapF inner outer = Wrap outer inner outer

instance (Format inner, Format outer) => Format (WrapF inner outer) where
  type R (WrapF inner outer) = R outer :%: R inner :%: R outer
  readpf (Wrap fl f fr) = do
    aoutl <- readpf fl
    ain <- readpf f
    aoutr <- readpf fr
    return (aoutl :%: ain :%: aoutr)

-- | Parse an optional value.

data MaybeF a = Maybe a

instance Format f => Format (MaybeF f) where
  type R (MaybeF f) = Maybe (R f)
  readpf (Maybe f) = (readpf f >>= return . Just) <++ return Nothing

-- | Parse one of the optional formats in a list.

data ChoiceF a = Choice [a]

instance Format f => Format (ChoiceF f) where
  type R (ChoiceF f) = R f
  readpf (Choice fs) = choice (readpf <$> fs)

-- | Parse one of two formats in a fully symmetric choice.

data EitherF a b = Either a b

instance (Format f1, Format f2) => Format (EitherF f1 f2) where
  type R (EitherF f1 f2) = Either (R f1) (R f2)
  readpf (Either f1 f2) = (Left <$> readpf f1) +++ (Right <$> readpf f2)

-- | Parse one of two formats, trying the left one first.

data EitherLF a b = EitherL a b

instance (Format f1, Format f2) => Format (EitherLF f1 f2) where
  type R (EitherLF f1 f2) = Either (R f1) (R f2)
  readpf (EitherL f1 f2) = (Left <$> readpf f1) <++ (Right <$> readpf f2)

--------------------------------------------------------------------------------

--
-- Tuple format descriptors: These all follow the same pattern.
--

instance
  (Format f1, Format f2)
  => Format
  (f1, f2)
  where
  type R (f1, f2) =
    (R f1, R f2)
  readpf (f1, f2) = do
    a1 <- readpf f1
    a2 <- readpf f2
    return (a1, a2)

instance
  (Format f1, Format f2, Format f3)
  => Format
  (f1, f2, f3)
  where
  type R (f1, f2, f3) =
    (R f1, R f2, R f3)
  readpf (f1, f2, f3) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    return (a1, a2, a3)

instance
  (Format f1, Format f2, Format f3, Format f4)
  => Format
  (f1, f2, f3, f4)
  where
  type R (f1, f2, f3, f4) =
    (R f1, R f2, R f3, R f4)
  readpf (f1, f2, f3, f4) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    return (a1, a2, a3, a4)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5)
  => Format
  (f1, f2, f3, f4, f5)
  where
  type R (f1, f2, f3, f4, f5) =
    (R f1, R f2, R f3, R f4, R f5)
  readpf (f1, f2, f3, f4, f5) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    return (a1, a2, a3, a4, a5)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6)
  => Format
  (f1, f2, f3, f4, f5, f6)
  where
  type R (f1, f2, f3, f4, f5, f6) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6)
  readpf (f1, f2, f3, f4, f5, f6) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    return (a1, a2, a3, a4, a5, a6)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7)
  => Format
  (f1, f2, f3, f4, f5, f6, f7)
  where
  type R (f1, f2, f3, f4, f5, f6, f7) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7)
  readpf (f1, f2, f3, f4, f5, f6, f7) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    return (a1, a2, a3, a4, a5, a6, a7)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    return (a1, a2, a3, a4, a5, a6, a7, a8)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9, R f10)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    a10 <- readpf f10
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9, R f10,
     R f11)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    a10 <- readpf f10
    a11 <- readpf f11
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9, R f10,
     R f11, R f12)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    a10 <- readpf f10
    a11 <- readpf f11
    a12 <- readpf f12
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12, Format f13)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9, R f10,
     R f11, R f12, R f13)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    a10 <- readpf f10
    a11 <- readpf f11
    a12 <- readpf f12
    a13 <- readpf f13
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12, Format f13, Format f14)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9, R f10,
     R f11, R f12, R f13, R f14)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    a10 <- readpf f10
    a11 <- readpf f11
    a12 <- readpf f12
    a13 <- readpf f13
    a14 <- readpf f14
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12, Format f13, Format f14,
   Format f15)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
  where
  type R (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15) =
    (R f1, R f2, R f3, R f4, R f5,
     R f6, R f7, R f8, R f9, R f10,
     R f11, R f12, R f13, R f14, R f15)
  readpf (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15) = do
    a1 <- readpf f1
    a2 <- readpf f2
    a3 <- readpf f3
    a4 <- readpf f4
    a5 <- readpf f5
    a6 <- readpf f6
    a7 <- readpf f7
    a8 <- readpf f8
    a9 <- readpf f9
    a10 <- readpf f10
    a11 <- readpf f11
    a12 <- readpf f12
    a13 <- readpf f13
    a14 <- readpf f14
    a15 <- readpf f15
    return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

