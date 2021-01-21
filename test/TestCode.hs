{-# LANGUAGE OverloadedStrings #-}

module TestCode (tests) where

import Code
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import qualified Model as Md
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.XML (def, parseText, readFile)
import Xsd (getSchema)

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
                CodeType
                  "AddresseeIDType"
                  "Name code type"
                  ( V.fromList
                      [ Code "01" "Proprietary" "Note that <IDTypeName> is required with proprietary identifiers",
                        Code "02" "Proprietary" "DEPRECATED \8211 use 01"
                      ]
                  )
                  False
                  []
          assertEqual "can derive description from type" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_territorycodelist.xsd"
          let actual = (topLevelTypeToCode scm . head . collectTypes) scm
              expected =
                CodeType
                  "TerritoryCodeList"
                  "Name code type"
                  ( V.fromList
                      [ Code "01" "Proprietary" "Note that <IDTypeName> is required with proprietary identifiers",
                        Code "02" "Proprietary" "DEPRECATED \8211 use 01"
                      ]
                  )
                  False
                  []
          assertEqual "can parse territory code list" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_space_separated.xsd"
          let actual = (topLevelElementToCode scm . head . collectCodes) scm
              expected =
                CodeType
                  "MyIDType"
                  "Name code type"
                  ( V.fromList
                      [ Code "01" "Proprietary" "Note that <IDTypeName> is required with proprietary identifiers",
                        Code "02" "Proprietary" "DEPRECATED \8211 use 01"
                      ]
                  )
                  True
                  []

          assertEqual "can parse territory code list" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_attributes.xsd"
          let actual = (topLevelAttributeCode scm . head . collectAttributes) scm
              expected =
                CodeType "TextFormatCode" "has not document" (V.fromList []) False []
          assertEqual "can parse attributes" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_general_attributes.xsd"
          let actual = (topLevelElementToCode scm . head . collectCodes) scm
              expected =
                CodeType
                  { xmlReferenceName = "MyIDType",
                    description = "Name code type",
                    codes = V.fromList [Code {value = "01", codeDescription = "Proprietary", notes = "Note that <IDTypeName> is required with proprietary identifiers"}, Code {value = "02", codeDescription = "Proprietary", notes = "DEPRECATED \8211 use 01"}],
                    spaceSeparatable = True,
                    elements =
                      [ Md.Model {Md.shortname = "textformat", Md.xmlReferenceName = "Textformat", Md.typeName = Just "TextFormatCode", Md.kind = Md.Attribute, Md.optional = True, Md.iterable = False, Md.elements = []},
                        Md.Model {Md.shortname = "sourcename", Md.xmlReferenceName = "Sourcename", Md.typeName = Just "string", Md.kind = Md.Attribute, Md.optional = True, Md.iterable = False, Md.elements = []}
                      ]
                  }
          assertEqual "can parse general attributes" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_code_refname.xsd"
          let actual = (topLevelElementToCode scm . head . collectCodes) scm
              expected =
                CodeType
                  { xmlReferenceName = "AVItemIDType",
                    description = "Name code type",
                    codes = V.fromList [Code {value = "01", codeDescription = "Proprietary", notes = "Note that <IDTypeName> is required with proprietary identifiers"}, Code {value = "02", codeDescription = "Proprietary", notes = "DEPRECATED \8211 use 01"}],
                    spaceSeparatable = True,
                    elements =
                      [ Md.Model
                          { Md.shortname = "sourcename",
                            Md.xmlReferenceName = "Sourcename",
                            Md.typeName = Just "string",
                            Md.kind = Md.Attribute,
                            Md.optional = True,
                            Md.iterable = False,
                            Md.elements = []
                          }
                      ]
                  }
          assertEqual "can parse enumrationed code refname" expected actual
      )
  ]
