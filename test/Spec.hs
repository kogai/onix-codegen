module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
  ( Counts (errors, failures),
    Test (..),
    Testable (test),
    runTestTT,
  )
import qualified TestModel as M
import qualified TestParser as P
import qualified TestUtils as U

main :: IO ()
main = do
  counts2 <- runTestTT (test $ M.tests ++ P.tests ++ U.utilTests)

  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
