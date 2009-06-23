
--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2009 Sean Leather
-- License     :  BSD3
--
-- Maintainer  :  leather@cs.uu.nl
--
-- Tests for Text.XFormat.*.
--------------------------------------------------------------------------------

module Main where

import qualified Read (test)
import qualified Show (test)

test :: Bool
test = and
  [ Read.test
  , Show.test
  ]

main :: IO ()
main =
  do putStrLn "Running tests for Text.XFormat.* ..."
     putStrLn $ "Result: " ++ if test then "Passed. :)" else "FAILED!!! :("

