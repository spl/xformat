{-# OPTIONS -fno-warn-type-defaults #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Show
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
--
-- Tests for Text.XFormat.Show.
--------------------------------------------------------------------------------

module Show (test) where

--------------------------------------------------------------------------------

import Text.XFormat.Show

--------------------------------------------------------------------------------

test :: Bool
test = and
  [ testBasic
  , testClasses
  , testOther
  , testRecursive
  , testTuples
  ]

testBasic :: Bool
testBasic = and
  [ showsf "abc" "" == "abc"
  , showsf 'c' "" == "c"
  , showf Char 'a' == "a"
  , showf String "abc" == "abc"
  , showf Int 5 == "5"
  , showf Integer 999999999999999999 == "999999999999999999"
  , showf Float 9.2 == "9.2"
  , showf Double 5.5 == "5.5"
  ]

testClasses :: Bool
testClasses = and
  [ showf Show "99.9" == "\"99.9\""
  , showf Num 7734 == "7734"
  ]

testOther :: Bool
testOther = and
  [ let n = 15 in showf (Spaces n) == replicate n ' '
  ]

testRecursive :: Bool
testRecursive = and
  [ showf (Show % Int % String) 4.05 20 " blah" == "4.0520 blah"
  , showf (Wrap '(' Int ')') 24 == "(24)"
  , showf (Align L 5 String) "abc" == "abc  "
  , showf (Align R 5 Int) 999 == "  999"
  , showf (Align L 5 "1234567") == "1234567"
  ]

testTuples :: Bool
testTuples = and
  [ showf (Char, Char) 'a' 'b' == "ab"
  , showf (Char, Char, Char) 'a' 'b' 'c' == "abc"
  , showf (Char, Char, Char, Char) 'a' 'b' 'c' 'd' == "abcd"
  , showf (Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' == "abcde"
  , showf (Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' == "abcdef"
  , showf (Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' == "abcdefg"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' == "abcdefgh"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' == "abcdefghi"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' == "abcdefghij"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' == "abcdefghijk"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' == "abcdefghijkl"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' == "abcdefghijklm"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' == "abcdefghijklmn"
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' == "abcdefghijklmno"
{-
  -- More than 15 is not yet supported.
  , showf (Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char) 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' == "abcdefghijklmnop"
-}
  ]

