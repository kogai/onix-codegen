{-# LANGUAGE OverloadedStrings #-}

module TestModel (tests) where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Model (Kind (Tag), dropDuplicate, model, models, typeToText)
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

expected4 =
  TypeComplex
    ( ComplexType
        { complexAnnotations = [],
          complexContent =
            ContentSimple
              ( SimpleContentExtension
                  ( SimpleExtension
                      { simpleExtensionBase = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "List82"},
                        simpleExtensionAttributes =
                          [ InlineAttribute
                              ( AttributeInline
                                  { attributeInlineName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "refname"},
                                    attributeInlineType = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "NMTOKEN"}),
                                    attributeInlineFixed = Just "BibleContents",
                                    attributeInlineUse = Optional
                                  }
                              ),
                            InlineAttribute
                              ( AttributeInline
                                  { attributeInlineName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "shortname"},
                                    attributeInlineType = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "NMTOKEN"}),
                                    attributeInlineFixed = Just "b352",
                                    attributeInlineUse = Optional
                                  }
                              )
                          ]
                      }
                  )
              )
        }
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
          scm <- getSchema "./test/test_model_atomic.xsd"
          let actual = typeToText expected1
          assertEqual "can derive string type" "string" actual
      ),
    TestCase
      ( do
          scm <- getSchema "./test/test_model_list.xsd"
          print ""
          (print . schemaTypes) scm
          print ""
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple list type" expected2 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./test/test_model_atom_enum.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type of enum" expected3 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./test/test_model_atom_ref.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type which reference to other type" expected4 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./test/test_model_atom_ref.xsd"
          let actual = typeToText expected4
          assertEqual "can derive referenced type" "BibleContents" actual
      )
  ]
