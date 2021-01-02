module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
  ( Counts (errors, failures),
    Testable (test),
    runTestTT,
  )
import qualified TestParser as P
import TestUtils

main :: IO ()
main = do
  counts2 <- runTestTT (test P.tests)
  -- (test $ utilTests ++ P.tests)

  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
