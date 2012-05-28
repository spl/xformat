
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2009-2012 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
--
-- Tests for Text.XFormat.*.
--------------------------------------------------------------------------------

module Main where

import System.Exit (exitFailure, exitSuccess)

import qualified Read (test)
import qualified Show (test)

test :: Bool
test = and
  [ Read.test
  , Show.test
  ]

main :: IO ()
main = if test then exitSuccess else exitFailure

