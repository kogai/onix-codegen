{-# LANGUAGE OverloadedStrings #-}

module TestCode (tests) where

import Code (code, codeType, collectCodes, collectTypes, topLevelElementToCode, topLevelTypeToCode)
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.XML (def, parseText, readFile)
import Xsd

tests =
  [ TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_collect.xsd"
          let actual = (length . collectCodes) scm
          assertEqual "can parse simple atomic type" 1 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_description.xsd"
          let actual = (topLevelElementToCode scm . head . collectCodes) scm
              expected =
                codeType
                  "AddresseeIDType"
                  "Name code type"
                  [ code "01" "Proprietary" "Note that <IDTypeName> is required with proprietary identifiers",
                    code "02" "Proprietary" "DEPRECATED \8211 use 01"
                  ]
          assertEqual "can derive description from type" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_territorycodelist.xsd"
          let actual = (topLevelTypeToCode scm . head . collectTypes) scm
              expected =
                codeType
                  "TerritoryCodeList"
                  "Name code type"
                  [ code "01" "Proprietary" "Note that <IDTypeName> is required with proprietary identifiers",
                    code "02" "Proprietary" "DEPRECATED \8211 use 01"
                  ]
          assertEqual "can parse territory code list" expected actual
      )
  ]
