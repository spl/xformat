{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.XFormat.Show
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines an extensible, type-indexed function for showing
-- well-typed values with a format descriptor. This may be considered a Haskell
-- variant of the C @printf@ function.
--
-- If you are primarily interested in using this library, you will want to see
-- 'showsf' and 'showf', the more user-friendly functions.
--
-- If you are also interested in extending this library with your own format
-- descriptors, you should read about the 'Format' class.
--------------------------------------------------------------------------------

module Text.XFormat.Show (

  -- * The Classes

  Format(..),
  Apply(..),

  -- * The Functions

  showsf,
  showf,

  -- * Format Descriptors

  -- | These are used to indicate which values and types to show.

  -- ** Basic Format Descriptors

  CharF(..),
  IntF(..),
  IntegerF(..),
  FloatF(..),
  DoubleF(..),
  StringF(..),

  -- ** Class-based Format Descriptors

  ShowF(..),
  NumF(..),

  -- ** Recursive Format Descriptors

  (:%:)(..),
  (%),

  WrapF(..),
  AlignF(..),
  AlignChopF(..),
  Dir(..),

  -- ** Other Format Descriptors

  SpacesF(..),

  -- * Utilities for Defining Instances

  Id(..),
  Arr(..),
  (:.:)(..),
  (<>),

) where

--------------------------------------------------------------------------------

import Prelude hiding ((<>))

-- | This class provides the signature for an extensible, type-indexed function
-- that uses a format descriptor to print a variable number of well-typed
-- arguments to a string. The type variable @d@ is the format descriptor, and
-- the 'Functor' variable @f@ determines the type of the value to be shown.
--
-- An instance of @Format@ adds a (type) case to the function. Before defining
-- an instance, you must first define a format descriptor for your specific type
-- and expected input. The descriptor is often very simple. See the descriptors
-- in this module for examples.
--
-- Here is the instance for types that are instances of 'Prelude.Show'.
--
-- @
--   data 'ShowF' a = 'Show' -- Format descriptor
-- @
--
-- @
--   instance ('Prelude.Show' a) => Format ('ShowF' a) ('Arr' a) where
--     'showsf'' 'Show' = 'Arr' 'shows'
-- @
--
-- The 'Arr' type is one of several 'Functor' wrappers necessary for defining
-- these instances.

class (Functor f) => Format d f | d -> f where

  -- | Given a format descriptor @d@, return a 'Functor' wrapping a @'String' ->
  -- 'String'@ type. This function may not be very useful outside of defining an
  -- instance for 'Format'. Instead, consider using 'showsf' or 'showf'.

  showsf' :: d -> f ShowS

--------------------------------------------------------------------------------

-- | Given a format descriptor @d@, a variable number of arguments represented
-- by @a@ (and determined by @d@), and a 'String', return a 'String' result.
-- This function removes the 'Functor' wrappers from the output of 'showsf'' to
-- get the variable number of arguments.

showsf :: (Format d f, Apply f ShowS a) => d -> a
showsf d = apply (showsf' d)

-- | Given a format descriptor @d@ and a variable number of arguments
-- represented by @a@ (and determined by @d@), return a 'String' result. This
-- function is the same as 'showsf' but has already been applied to a 'String'
-- input.

showf :: (Format d f, Apply f String a) => d -> a
showf d = apply (fmap (\f -> f "") (showsf' d))

--------------------------------------------------------------------------------

--
-- Functor wrappers
--

-- | Wrapper for a format constant that does not take any arguments. Used in
-- @instance 'Format' 'String' Id@ for example.

newtype Id a = Id a

instance Functor Id where
  fmap f (Id x) = Id (f x)

-- | Wrapper for a format descriptor that takes an argument. Used in @instance
-- ('Prelude.Show' a) => 'Format' ('ShowF' a) (Arr a)@ for example.

newtype Arr a b = Arr (a -> b)

instance Functor (Arr a) where
  fmap f (Arr g) = Arr (f . g)

-- | Wrapper for a format descriptor that composes two descriptors. Used in
-- @instance ('Format' d1 f1, 'Format' d2 f2) => 'Format' (d1 :%: d2) (f1 :.:
-- f2)@ for example.

newtype (:.:) f g a = Comp (f (g a))

infixr 8 :.:

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp fga) = Comp (fmap (fmap f) fga)

-- | Helpful function for defining instances of composed format descriptors.

(<>) :: (Functor f, Functor g) => f (b -> c) -> g (a -> b) -> (:.:) f g (a -> c)
f <> g = Comp (fmap (\s -> fmap (\t -> s . t) g) f)
infixr 8 <>

--------------------------------------------------------------------------------

--
-- Functor wrapper removal
--

class (Functor f) => Apply f a b | f a -> b where
  apply :: f a -> b

instance Apply Id a a where
  apply (Id a) = a

instance Apply (Arr a) b (a -> b) where
  apply (Arr f) = f

instance (Apply f b c, Apply g a b) => Apply (f :.: g) a c where
  apply (Comp fga) = apply (fmap apply fga)

--------------------------------------------------------------------------------

--
-- Format constants
--
-- These are not descriptors in the traditional sense. These are constants that
-- are shown directly without taking arguments.
--

-- | Print the enclosed 'String'.

instance Format String Id where
  showsf' s = Id (showString s)

-- | Print the enclosed 'Char'.

instance Format Char Id where
  showsf' c = Id (showChar c)

--------------------------------------------------------------------------------

--
-- Basic format descriptors
--

-- | Print a character argument.

data CharF = Char

instance Format CharF (Arr Char) where
  showsf' Char = Arr showChar

-- | Print a string argument.

data StringF = String

instance Format StringF (Arr String) where
  showsf' String = Arr showString

-- | Print an 'Int' argument.

data IntF = Int

instance Format IntF (Arr Int) where
  showsf' Int = Arr shows

-- | Print an 'Integer' argument.

data IntegerF = Integer

instance Format IntegerF (Arr Integer) where
  showsf' Integer = Arr shows

-- | Print a 'Float' argument.

data FloatF = Float

instance Format FloatF (Arr Float) where
  showsf' Float = Arr shows

-- | Print a 'Double' argument.

data DoubleF = Double

instance Format DoubleF (Arr Double) where
  showsf' Double = Arr shows

--------------------------------------------------------------------------------

--
-- Class format descriptors
--

-- | Print an argument whose type is an instance of the class 'Prelude.Show'.

data ShowF a = Show

instance (Show a) => Format (ShowF a) (Arr a) where
  showsf' Show = Arr shows

-- | Print an argument whose type is an instance of the class 'Prelude.Num'.

data NumF a = Num

instance (Num a, Show a) => Format (NumF a) (Arr a) where
  showsf' Num = Arr shows

--------------------------------------------------------------------------------

--
-- Other format descriptors
--

-- | Print a specified number of spaces.

data SpacesF = Spaces Int

instance Format SpacesF Id where
  showsf' (Spaces n) = Id (showString (replicate n ' '))

--------------------------------------------------------------------------------

--
-- Recursive format descriptors
--

-- | Right-associative pair. First print a @a@-type format and then a @b@-type
-- format.

data a :%: b = a :%: b
  deriving (Eq, Show)

infixr 8 :%:

-- | Right-associative pair. This is a shorter, functional equivalent to the
-- type @(:%:)@.

(%) :: a -> b -> a :%: b
(%) = (:%:)

infixr 8 %

instance (Format d1 f1, Format d2 f2) => Format (d1 :%: d2) (f1 :.: f2) where
  showsf' (d1 :%: d2) = showsf' d1 <> showsf' d2

-- | Print a format of one type wrapped by two other formats of a different
-- type.

data WrapF inner outer = Wrap outer inner outer

instance (Format din fin, Format dout fout)
  => Format (WrapF din dout) (fout :.: fin :.: fout) where
  showsf' (Wrap doutl din doutr) = showsf' doutl <> showsf' din <> showsf' doutr

-- | Print a format aligned left or right within a column of the given width.

data AlignF a = Align Dir Int a

-- | Same as 'AlignF' but chop off the output if it extends past the column
-- width.

data AlignChopF a = AlignChop Dir Int a

-- | Direction (left or right) used for 'AlignF' and 'AlignChopF'.

data Dir = L | R

align :: Bool -> Dir -> Int -> ShowS -> ShowS
align doChop dir wid input =
  case dir of
    L -> chop (take wid)         . input . addSpaces
    R -> chop (drop (len - wid)) . addSpaces . input
  where
    len = length (input "")
    spaces = replicate (wid - len) ' '
    chop act = if doChop && len > wid then act else id
    addSpaces = if len < wid then showString spaces else id

instance (Format d f) => Format (AlignF d) f where
  showsf' (Align dir wid d) = fmap (align False dir wid) (showsf' d)

instance (Format d f) => Format (AlignChopF d) f where
  showsf' (AlignChop dir wid d) = fmap (align True dir wid) (showsf' d)

--------------------------------------------------------------------------------

--
-- Tuple format descriptors: These all follow the same pattern.
--

instance
  (Format d1 f1, Format d2 f2)
  => Format
  (d1, d2)
  (f1 :.: f2)
  where
  showsf' (d1, d2) =
    showsf' d1 <> showsf' d2

instance
  (Format d1 f1, Format d2 f2, Format d3 f3)
  => Format
  (d1, d2, d3)
  (f1 :.: f2 :.: f3)
  where
  showsf' (d1, d2, d3) =
    showsf' d1 <> showsf' d2 <> showsf' d3

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4)
  => Format
  (d1, d2, d3, d4)
  (f1 :.: f2 :.: f3 :.: f4)
  where
  showsf' (d1, d2, d3, d4) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5)
  => Format
  (d1, d2, d3, d4, d5)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5)
  where
  showsf' (d1, d2, d3, d4, d5) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6)
  => Format
  (d1, d2, d3, d4, d5, d6)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6)
  where
  showsf' (d1, d2, d3, d4, d5, d6) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7)
  => Format
  (d1, d2, d3, d4, d5, d6, d7)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9, Format d10 f10)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9 :.: f10)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9 <> showsf' d10

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9, Format d10 f10,
   Format d11 f11)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9 :.: f10 :.: f11)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9 <> showsf' d10 <>
    showsf' d11

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9, Format d10 f10,
   Format d11 f11, Format d12 f12)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9 :.: f10 :.:
   f11 :.: f12)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9 <> showsf' d10 <>
    showsf' d11 <> showsf' d12

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9, Format d10 f10,
   Format d11 f11, Format d12 f12, Format d13 f13)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9 :.: f10 :.:
   f11 :.: f12 :.: f13)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9 <> showsf' d10 <>
    showsf' d11 <> showsf' d12 <> showsf' d13

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9, Format d10 f10,
   Format d11 f11, Format d12 f12, Format d13 f13, Format d14 f14)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9 :.: f10 :.:
   f11 :.: f12 :.: f13 :.: f14)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9 <> showsf' d10 <>
    showsf' d11 <> showsf' d12 <> showsf' d13 <> showsf' d14

instance
  (Format d1 f1, Format d2 f2, Format d3 f3, Format d4 f4, Format d5 f5,
   Format d6 f6, Format d7 f7, Format d8 f8, Format d9 f9, Format d10 f10,
   Format d11 f11, Format d12 f12, Format d13 f13, Format d14 f14,
   Format d15 f15)
  => Format
  (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15)
  (f1 :.: f2 :.: f3 :.: f4 :.: f5 :.: f6 :.: f7 :.: f8 :.: f9 :.: f10 :.:
   f11 :.: f12 :.: f13 :.: f14 :.: f15)
  where
  showsf' (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15) =
    showsf' d1 <> showsf' d2 <> showsf' d3 <> showsf' d4 <> showsf' d5 <>
    showsf' d6 <> showsf' d7 <> showsf' d8 <> showsf' d9 <> showsf' d10 <>
    showsf' d11 <> showsf' d12 <> showsf' d13 <> showsf' d14 <> showsf' d15

