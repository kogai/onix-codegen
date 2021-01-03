{-# LANGUAGE OverloadedStrings #-}

module TestModel (tests) where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Model (Kind (Tag), dropDuplicate, model, models)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.XML (def, parseText, readFile)
import Xsd

expected1 =
  TypeSimple
    ( AtomicType
        SimpleRestriction
          { simpleRestrictionBase = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "string"}),
            simpleRestrictionConstraints = []
          }
        []
    )

expected2 =
  TypeSimple
    ( ListType
        ( Ref
            ( QName
                { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
                  qnName = "List91"
                }
            )
        )
        []
    )

expected3 =
  TypeSimple
    ( AtomicType
        ( SimpleRestriction
            { simpleRestrictionBase = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "string"}),
              simpleRestrictionConstraints =
                [ Enumeration "01",
                  Enumeration "02",
                  Enumeration "03",
                  Enumeration "04",
                  Enumeration "05",
                  Enumeration "08",
                  Enumeration "09",
                  Enumeration "12",
                  Enumeration "13",
                  Enumeration "14",
                  Enumeration "88",
                  Enumeration "89"
                ]
            }
        )
        [Documentation "Notification or update type code"]
    )

tests =
  [ TestCase
      ( do
          scm <- getSchema "./test/test_model_atomic.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type" expected1 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./test/test_model_list.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple list type" expected2 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./test/test_model_atom_enum.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type with union" expected3 actual
      )
  ]
