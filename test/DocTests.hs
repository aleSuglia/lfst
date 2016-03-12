module Main where

import           Test.DocTest

main :: IO ()
main = doctest ["-isrc", "Algebra/LFST/Membership.hs", "Algebra/LFST/FuzzySet.hs"]
