module Main where

import           Test.DocTest

main :: IO ()
main = doctest ["-isrc", "FST/Membership.hs", "FST/FuzzySet.hs"]
