{-# LANGUAGE OverloadedStrings #-}

module TestCode (tests) where

import Code (collectCodes)
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.XML (def, parseText, readFile)
import Xsd

tests =
  [ TestCase
      ( do
          scm <- getSchema "./test/test_code.xsd"
          let actual = (length . collectCodes) scm
          assertEqual "can parse simple atomic type" 1 actual
      )
  ]
