{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012 Sean Leather
-- License     :  BSD3
--
-- Benchmarks
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Criterion.Main

import qualified Text.XFormat.Show as S1
import qualified ShowGADT as S2

--------------------------------------------------------------------------------

str03 = ("abc","def","ghi")
str09 = ("abc","def","ghi","jkl","mno","pqr","stu","vwx","yz0")
str15 = ("abc","def","ghi","jkl","mno","pqr","stu","vwx","yz0","123","456","789","ABC","DEF","GHI")

s1one03 = ("abc",S1.String,"ghi")
s2one03 = ("abc",S2.String,"ghi")
s1one09 = ("abc","def","ghi","jkl",S1.String,"pqr","stu","vwx","yz0")
s2one09 = ("abc","def","ghi","jkl",S2.String,"pqr","stu","vwx","yz0")
s1one15 = ("abc","def","ghi","jkl","mno","pqr","stu",S1.String,"yz0","123","456","789","ABC","DEF","GHI")
s2one15 = ("abc","def","ghi","jkl","mno","pqr","stu",S2.String,"yz0","123","456","789","ABC","DEF","GHI")

main :: IO ()
main = defaultMain
  [ bgroup "str" [ bgroup "03" [ bench "vanilla" (nf S1.showf str03)
                               , bench "GADT"    (nf S2.showf str03)
                               ]
                 , bgroup "09" [ bench "vanilla" (nf S1.showf str09)
                               , bench "GADT"    (nf S2.showf str09)
                               ]
                 , bgroup "15" [ bench "vanilla" (nf S1.showf str15)
                               , bench "GADT"    (nf S2.showf str15)
                               ]
                 ]
  , bgroup "one" [ bgroup "03" [ bench "vanilla" (nf (S1.showf s1one03) "def")
                               , bench "GADT"    (nf (S2.showf s2one03) "def")
                               ]
                 , bgroup "09" [ bench "vanilla" (nf (S1.showf s1one09) "mno")
                               , bench "GADT"    (nf (S2.showf s2one09) "mno")
                               ]
                 , bgroup "15" [ bench "vanilla" (nf (S1.showf s1one15) "vwx")
                               , bench "GADT"    (nf (S2.showf s2one15) "vwx")
                               ]
                 ]
  ]

