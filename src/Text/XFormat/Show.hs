{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.XFormat.Show
-- Copyright   :  (c) 2009-2012 Sean Leather
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


-- | This class provides the signature for an extensible, type-indexed function
-- that uses a format descriptor to print a variable number of well-typed
-- arguments to a string. The type variable @f@ is the format descriptor, and
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

class Functor (F f) => Format f where
  type F f :: * -> *

  -- | Given a format descriptor @f@, the result type @F f@ is a functor whose
  -- type parameter is @'String' -> 'String'@.

  showsf' :: f -> F f ShowS

--------------------------------------------------------------------------------

-- | Given a format descriptor @fmt@, a variable number of arguments represented
-- by @a@ (and determined by @fmt@), and a 'String', return a 'String' result.
-- This function removes the 'Functor' wrappers from the output of 'showsf'' to
-- get the variable number of arguments.

showsf :: (Format f, Apply (F f)) => f -> A (F f) ShowS
showsf fmt = apply (showsf' fmt)

-- | Given a format descriptor @fmt@ and a variable number of arguments
-- represented by @a@ (and determined by @fmt@), return a 'String' result. This
-- function is the same as 'showsf' but has already been applied to a 'String'
-- input.

showf :: (Format f, Apply (F f)) => f -> A (F f) String
showf fmt = apply (fmap (\f -> f "") (showsf' fmt))

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

type family A (f :: * -> *) a :: *
type instance A Id        a  = a
type instance A (Arr a)   b  = a -> b
type instance A (f :.: g) a  = A f (A g a)

class Functor f => Apply f                     where apply :: f a -> A f a
instance Apply Id                              where apply (Id a)     = a
instance Apply (Arr a)                         where apply (Arr f)    = f
instance (Apply f, Apply g) => Apply (f :.: g) where apply (Comp fg)  = apply (fmap apply fg)

--------------------------------------------------------------------------------

--
-- Format constants
--
-- These are not descriptors in the traditional sense. These are constants that
-- are shown directly without taking arguments.
--

-- | Print the enclosed 'String'.

instance Format String where
  type F String = Id
  showsf' s = Id (showString s)

-- | Print the enclosed 'Char'.

instance Format Char where
  type F Char = Id
  showsf' c = Id (showChar c)

--------------------------------------------------------------------------------

--
-- Basic format descriptors
--

-- | Print a character argument.

data CharF = Char

instance Format CharF where
  type F CharF = Arr Char
  showsf' Char = Arr showChar

-- | Print a string argument.

data StringF = String

instance Format StringF where
  type F StringF = Arr String
  showsf' String = Arr showString

-- | Print an 'Int' argument.

data IntF = Int

instance Format IntF where
  type F IntF = Arr Int
  showsf' Int = Arr shows

-- | Print an 'Integer' argument.

data IntegerF = Integer

instance Format IntegerF where
  type F IntegerF = Arr Integer
  showsf' Integer = Arr shows

-- | Print a 'Float' argument.

data FloatF = Float

instance Format FloatF where
  type F FloatF = Arr Float
  showsf' Float = Arr shows

-- | Print a 'Double' argument.

data DoubleF = Double

instance Format DoubleF where
  type F DoubleF = Arr Double
  showsf' Double = Arr shows

--------------------------------------------------------------------------------

--
-- Class format descriptors
--

-- | Print an argument whose type is an instance of the class 'Prelude.Show'.

data ShowF a = Show

instance (Show a) => Format (ShowF a) where
  type F (ShowF a) = Arr a
  showsf' Show = Arr shows

-- | Print an argument whose type is an instance of the class 'Prelude.Num'.

data NumF a = Num

instance (Num a, Show a) => Format (NumF a) where
  type F (NumF a) = Arr a
  showsf' Num = Arr shows

--------------------------------------------------------------------------------

--
-- Other format descriptors
--

-- | Print a specified number of spaces.

data SpacesF = Spaces Int

instance Format SpacesF where
  type F SpacesF = Id
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

instance (Format f, Format g) => Format (f :%: g) where
  type F (f :%: g) = F f :.: F g
  showsf' (f :%: g) = showsf' f <> showsf' g

-- | Print a format of one type wrapped by two other formats of a different
-- type.

data WrapF inner outer = Wrap outer inner outer

instance (Format din, Format dout) => Format (WrapF din dout) where
  type F (WrapF din dout) = F dout :.: F din :.: F dout
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

instance Format f => Format (AlignF f) where
  type F (AlignF f) = F f
  showsf' (Align dir wid f) = fmap (align False dir wid) (showsf' f)

instance Format f => Format (AlignChopF f) where
  type F (AlignChopF f) = F f
  showsf' (AlignChop dir wid f) = fmap (align True dir wid) (showsf' f)

--------------------------------------------------------------------------------

--
-- Tuple format descriptors
--

instance
  (Format f1, Format f2)
  => Format
  (f1, f2)
  where
  type F (f1, f2) =
    F f1 :.: F f2
  showsf' (f1, f2) =
    showsf' f1 <> showsf' f2

instance
  (Format f1, Format f2, Format f3)
  => Format
  (f1, f2, f3)
  where
  type F (f1, f2, f3) =
    F f1 :.: F f2 :.: F f3
  showsf' (f1, f2, f3) =
    showsf' f1 <> showsf' f2 <> showsf' f3

instance
  (Format f1, Format f2, Format f3, Format f4)
  => Format
  (f1, f2, f3, f4)
  where
  type F (f1, f2, f3, f4) =
    F f1 :.: F f2 :.: F f3 :.: F f4
  showsf' (f1, f2, f3, f4) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5)
  => Format
  (f1, f2, f3, f4, f5)
  where
  type F (f1, f2, f3, f4, f5) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5
  showsf' (f1, f2, f3, f4, f5) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6)
  => Format
  (f1, f2, f3, f4, f5, f6)
  where
  type F (f1, f2, f3, f4, f5, f6) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6
  showsf' (f1, f2, f3, f4, f5, f6) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7)
  => Format
  (f1, f2, f3, f4, f5, f6, f7)
  where
  type F (f1, f2, f3, f4, f5, f6, f7) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7
  showsf' (f1, f2, f3, f4, f5, f6, f7) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9 :.: F f10
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9 <> showsf' f10

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9 :.: F f10 :.:
    F f11
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9 <> showsf' f10 <>
    showsf' f11

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9 :.: F f10 :.:
    F f11 :.: F f12
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9 <> showsf' f10 <>
    showsf' f11 <> showsf' f12

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12, Format f13)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9 :.: F f10 :.:
    F f11 :.: F f12 :.: F f13
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9 <> showsf' f10 <>
    showsf' f11 <> showsf' f12 <> showsf' f13

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12, Format f13, Format f14)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9 :.: F f10 :.:
    F f11 :.: F f12 :.: F f13 :.: F f14
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9 <> showsf' f10 <>
    showsf' f11 <> showsf' f12 <> showsf' f13 <> showsf' f14

instance
  (Format f1, Format f2, Format f3, Format f4, Format f5,
   Format f6, Format f7, Format f8, Format f9, Format f10,
   Format f11, Format f12, Format f13, Format f14,
   Format f15)
  => Format
  (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
  where
  type F (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15) =
    F f1 :.: F f2 :.: F f3 :.: F f4 :.: F f5 :.:
    F f6 :.: F f7 :.: F f8 :.: F f9 :.: F f10 :.:
    F f11 :.: F f12 :.: F f13 :.: F f14 :.: F f15
  showsf' (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15) =
    showsf' f1 <> showsf' f2 <> showsf' f3 <> showsf' f4 <> showsf' f5 <>
    showsf' f6 <> showsf' f7 <> showsf' f8 <> showsf' f9 <> showsf' f10 <>
    showsf' f11 <> showsf' f12 <> showsf' f13 <> showsf' f14 <> showsf' f15

