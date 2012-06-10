{-# OPTIONS -fno-warn-type-defaults #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Read
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
--
-- Tests for Text.XFormat.Read.
--------------------------------------------------------------------------------

module Read (test) where

--------------------------------------------------------------------------------

import Text.XFormat.Read

--------------------------------------------------------------------------------

test :: Bool
test = and
  [ testBasic
  , testClasses
  , testRecursive
  , testTuples
  ]

testBasic :: Bool
testBasic = and
  [ readsf Int "5abc" == [(5, "abc")]
  , readsf 'a' "5" == []
  , readsf 'a' "a" == [('a', "")]
  , readf "Hello" "Hello" == Just "Hello"
  , readf "Hello" "Goodbye" == Nothing
  ]

testClasses :: Bool
testClasses = and
  [ readf Read "\"34\"" == Just "34"
  , readf Num "3.4" == Just 3.4
  ]

testRecursive :: Bool
testRecursive = and
  [ readf (Char % Integer % Space % Float % Space % Double % Rest) "~99 9.9 0.3" ==
      Just ('~' :%: (99 :%: (" " :%: (9.9 :%: (" " :%: (0.3 :%: ""))))))
  , readf String "" == Nothing
  , readf String "a" == Just "a"
  , readf (parens Int) "(1)" == Just 1
  , readf (quotes Int) "\"1\"" == Just 1
  , readf (brackets Int) "[1]" == Just 1
  , readf (braces Int) "{1}" == Just 1
  , readf (Maybe Int) "1" == Just (Just 1)
  , readf (Maybe Int) "a" == Just Nothing
  , readf (Choice ['(',')']) ")" == Just ')'
  , readf (Either Char Int) "1" == Just (Left '1')
  , readf (EitherL Int Char) "1" == Just (Left 1)
  , readf (Either Int Char) "a1" == Just (Right 'a')
  ]

testTuples :: Bool
testTuples = and
  [ readf (Char, Char) "ab" == Just ('a', 'b')
  , readf (Char, Char, Char) "abc" == Just ('a', 'b', 'c')
  , readf (Char, Char, Char, Char) "abcd" == Just ('a', 'b', 'c', 'd')
  , readf (Char, Char, Char, Char, Char) "abcde" == Just ('a', 'b', 'c', 'd', 'e')
  , readf (Char, Char, Char, Char, Char, Char) "abcdef" == Just ('a', 'b', 'c', 'd', 'e', 'f')
  , readf (Char, Char, Char, Char, Char, Char, Char) "abcdefg" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char) "abcdefgh" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghi" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghij" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghijk" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghijkl" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghijklm" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghijklmn" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n')
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghijklmno" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o')
{-
  -- More than 15 is not yet supported.
  , readf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) "abcdefghijklmnop" == Just ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p')
-}
  ]

