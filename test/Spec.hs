module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
  ( Counts (errors, failures),
    Test (..),
    Testable (test),
    runTestTT,
  )
import qualified TestCode as C
import qualified TestMixed as Mi
import qualified TestModel as M
import qualified TestParser as P

main :: IO ()
main = do
  counts2 <- runTestTT (test $ M.tests ++ P.tests ++ C.tests ++ Mi.tests)

  if errors counts2 + failures counts2 == 0
    then exitSuccess
    else exitFailure
