{-# LANGUAGE OverloadedStrings #-}

module TestParser (tests) where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Model (Kind (Tag), dropDuplicate, model, models)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import TestUtils (makeTargetQName)
import Xsd

expected =
  ElementInline
    { elementName = makeTargetQName "ONIXMessage",
      elementType =
        Inline
          ( TypeComplex
              ( ComplexType
                  { complexAnnotations = [],
                    complexContent =
                      ContentPlain
                        ( PlainContent
                            { plainContentModel =
                                Just
                                  ( Sequence
                                      [ Inline
                                          ( ChoiceOfSequence
                                              (Occurs (1, MaxOccurs 1))
                                              [ Inline
                                                  ( ElementOfChoice
                                                      (Occurs (1, MaxOccurs 1))
                                                      [ RefElement
                                                          ( ElementRef
                                                              { elementRefName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "Header"},
                                                                elementRefOccurs = Occurs (1, MaxOccurs 1)
                                                              }
                                                          )
                                                      ]
                                                  ),
                                                Inline
                                                  ( ElementOfChoice
                                                      (Occurs (1, MaxOccursUnbound))
                                                      [ RefElement (ElementRef {elementRefName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "Product"}, elementRefOccurs = Occurs (1, MaxOccurs 1)}),
                                                        RefElement (ElementRef {elementRefName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "MainSeriesRecord"}, elementRefOccurs = Occurs (1, MaxOccurs 1)}),
                                                        RefElement (ElementRef {elementRefName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "SubSeriesRecord"}, elementRefOccurs = Occurs (1, MaxOccurs 1)})
                                                      ]
                                                  )
                                              ]
                                          )
                                      ]
                                  ),
                              plainContentAttributes =
                                [ InlineAttribute
                                    ( AttributeInline
                                        { attributeInlineName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "refname"},
                                          attributeInlineType = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "NMTOKEN"}),
                                          attributeInlineFixed = Just "ONIXMessage",
                                          attributeInlineUse = Optional
                                        }
                                    ),
                                  InlineAttribute
                                    ( AttributeInline
                                        { attributeInlineName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "shortname"},
                                          attributeInlineType = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "NMTOKEN"}),
                                          attributeInlineFixed = Just "ONIXmessage",
                                          attributeInlineUse = Optional
                                        }
                                    ),
                                  InlineAttribute
                                    ( AttributeInline
                                        { attributeInlineName = QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "release"},
                                          attributeInlineType = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "string"}),
                                          attributeInlineFixed = Just "2.1",
                                          attributeInlineUse = Optional
                                        }
                                    ),
                                  AttributeGroupRef (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "generalAttributes"})
                                ]
                            }
                        ),
                    complexMixed = False
                  }
              )
          ),
      elementOccurs = Occurs (1, MaxOccurs 1),
      elementNillable = False,
      elementAnnotations = []
    }

tests =
  [ TestCase
      ( do
          scm <- getSchema "./fixtures/test_parser.xsd"
          let actual = (head . map snd . M.toList . schemaElements) scm
          assertEqual "dropDuplicate" expected actual
      )
  ]
