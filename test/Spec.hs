module Main (main) where

import System.Exit
import Test.HUnit
import TestUtils

main :: IO ()
main = do
  counts2 <-
    runTestTT
      (test utilTests)

  if (errors counts2 + failures counts2 == 0)
    then exitSuccess
    else exitFailure
