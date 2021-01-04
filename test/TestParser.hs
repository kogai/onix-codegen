{-# LANGUAGE OverloadedStrings #-}

module TestParser (tests) where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Model (Kind (Tag), dropDuplicate, model, models)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Xsd

expected =
  Element
    { elementName =
        QName
          { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
            qnName = "ONIXMessage"
          },
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
                                              [ Inline
                                                  ( ElementOfChoice
                                                      [ Ref
                                                          ( QName
                                                              { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
                                                                qnName = "Header"
                                                              }
                                                          )
                                                      ]
                                                  ),
                                                Inline
                                                  ( ElementOfChoice
                                                      [ Ref
                                                          ( QName
                                                              { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
                                                                qnName = "Product"
                                                              }
                                                          ),
                                                        Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "MainSeriesRecord"}),
                                                        Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}), qnName = "SubSeriesRecord"})
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
                                    )
                                ]
                            }
                        )
                  }
              )
          ),
      elementOccurs = (1, MaxOccurs 1),
      elementNillable = False,
      elementAnnotations = []
    }

tests =
  [ TestCase
      ( do
          scm <- getSchema "./test/test_parser.xsd"
          let actual = (head . map snd . M.toList . schemaElements) scm
          assertEqual "dropDuplicate" expected actual
      )
  ]
