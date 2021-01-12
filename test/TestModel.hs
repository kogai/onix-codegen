{-# LANGUAGE OverloadedStrings #-}

module TestModel (tests) where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Model
import qualified Model as Md
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import TestUtils (makeTargetQName)
import Text.XML (def, parseText, readFile)
import Util
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
        (Ref $ makeTargetQName "List91")
        []
    )

expected3 =
  TypeSimple
    ( AtomicType
        ( SimpleRestriction
            { simpleRestrictionBase = Ref (QName {qnNamespace = Just (Namespace {fromNamespace = "http://www.w3.org/2001/XMLSchema"}), qnName = "string"}),
              simpleRestrictionConstraints =
                [ Enumeration "01" [Documentation "Early notification", Documentation "Use for a complete record issued earlier than approximately six months before publication"],
                  Enumeration "02" [Documentation "Advance notification (confirmed)", Documentation "Use for a complete record issued to confirm advance information approximately six months before publication; or for a complete record issued after that date and before information has been confirmed from the book-in-hand"],
                  Enumeration "03" [Documentation "Notification confirmed on publication", Documentation "Use for a complete record issued to confirm advance information at or just before actual publication date; or for a complete record issued at any later date"],
                  Enumeration "04" [Documentation "Update (partial)", Documentation "In ONIX 3.0 only, use when sending a \8216block update\8217 record. In previous ONIX releases, ONIX updating has generally been by complete record replacement using code 03, and code 04 is not used"],
                  Enumeration "05" [Documentation "Delete", Documentation "Use when sending an instruction to delete a record which was previously issued. Note that a Delete instruction should NOT be used when a product is cancelled, put out of print, or otherwise withdrawn from sale: this should be handled as a change of Publishing status, leaving the receiver to decide whether to retain or delete the record. A Delete instruction is only used when there is a particular reason to withdraw a record completely, eg because it was issued in error"],
                  Enumeration "08" [Documentation "Notice of sale", Documentation "Notice of sale of a product, from one publisher to another: sent by the publisher disposing of the product"],
                  Enumeration "09" [Documentation "Notice of acquisition", Documentation "Notice of acquisition of a product, by one publisher from another: sent by the acquiring publisher"],
                  Enumeration "12" [Documentation "Update \8211 SupplyDetail only", Documentation "ONIX Books 2.1 supply update \8211 <SupplyDetail> only (not used in ONIX 3.0)"],
                  Enumeration "13" [Documentation "Update \8211 MarketRepresentation only", Documentation "ONIX Books 2.1 supply update \8211 <MarketRepresentation> only (not used in ONIX 3.0)"],
                  Enumeration "14" [Documentation "Update \8211 SupplyDetail and MarketRepresentation", Documentation "ONIX Books 2.1 supply update \8211 both <SupplyDetail> and <MarketRepresentation> (not used in ONIX 3.0)"],
                  Enumeration "88" [Documentation "Test update (Partial)", Documentation "ONIX 3.0 only. Record may be processed for test purposes, but data should be discarded. Sender must ensure the <RecordReference> matches a previously-sent Test record"],
                  Enumeration "89" [Documentation "Test record", Documentation "Record may be processed for test purposes, but data should be discarded. Sender must ensure the <RecordReference> does not match any previously-sent live product record"]
                ]
            }
        )
        [Documentation "Notification or update type code"]
    )

expected4 =
  TypeComplex
    ( ComplexType
        { complexAnnotations = [],
          complexMixed = False,
          complexContent =
            ContentSimple
              ( SimpleContentExtension
                  ( SimpleExtension
                      { simpleExtensionBase = makeTargetQName "List82",
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
          scm <- getSchema "./fixtures/test_model_atomic.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type" expected1 actual
      ),
    TestCase
      ( assertEqual
          "dropDuplicate"
          [ model "m1" "M1" (Just "string") Tag False False [],
            model "m2" "M2" (Just "string") Tag False False [],
            model "m3" "M3" (Just "string") Tag True False [],
            model "m4" "M4" (Just "string") Tag False False [],
            model "m5" "M5" (Just "string") Tag False False [],
            model "m6" "M6" (Just "string") Tag False False []
          ]
          ( dropDuplicate
              [ model "m1" "M1" (Just "string") Tag False False [],
                model "m2" "M2" (Just "string") Tag False False [],
                model "m3" "M3" (Just "string") Tag False False [],
                model "m4" "M4" (Just "string") Tag False False [],
                model "m5" "M5" (Just "string") Tag False False [],
                model "m6" "M6" (Just "string") Tag False False [],
                model "m3" "M3" (Just "string") Tag False False [],
                model "m3" "M3" (Just "string") Tag False False []
              ]
          )
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_atomic.xsd"
          let actual = typeToText expected1
          assertEqual "can derive string type" "string" actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_list.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple list type" expected2 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_atom_enum.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type of enum" expected3 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_atom_ref_bylist.xsd"
          let actual = (head . map snd . M.toList . schemaTypes) scm
          assertEqual "can parse simple atomic type which reference to other type" expected4 actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_atom_ref_bylist.xsd"
          let actual = typeToText expected4
          assertEqual "can derive referenced type" "BibleContents" actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_atom_ref_byname.xsd"
          let actual = (typeToText . head . map snd . M.toList . schemaTypes) scm
          assertEqual "can derive referenced type" "TerritoryCodeList" actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_atom_nonempty.xsd"
          let actual = (typeToText . head . map snd . M.toList . schemaTypes) scm
          assertEqual "can derive referenced type" "string" actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_element.xsd"
          let key = makeTargetQName "OnOrderDetail"
              actual = (topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              expected =
                model
                  "onorderdetail"
                  "OnOrderDetail"
                  Nothing
                  Tag
                  False
                  False
                  [ model "j351" "OnOrder" (Just "string") Tag False False [],
                    model "j302" "ExpectedDate" (Just "string") Tag False False []
                  ]
          assertEqual "can derive field of type" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_choice.xsd"
          let key = makeTargetQName "ConferenceSponsor"
              actual = (topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              expected =
                model
                  "conferencesponsor"
                  "ConferenceSponsor"
                  Nothing
                  Tag
                  False
                  False
                  [ model "b036" "PersonName" (Just "string") Tag True False [],
                    model "b047" "CorporateName" (Just "string") Tag True False [],
                    model "conferencesponsoridentifier" "ConferenceSponsorIdentifier" (Just "string") Tag True False []
                  ]
          assertEqual "can parse sum type" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_mixed_html.xsd"
          let key = makeTargetQName "Annotation"
              actual = collectElements scm
          assertEqual "can parse choice of html string" [] actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_iterable.xsd"
          let key = makeTargetQName "Bible"
              es = (Md.elements . topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              a100 = head es
              a101 = es !! 1
              a102 = es !! 2

          assertEqual "a100 optional" True (Md.optional a100)
          assertEqual "a100 iterable" False (Md.iterable a100)
          assertEqual "a101 optional" True (Md.optional a101)
          assertEqual "a101 iterable" True (Md.iterable a101)
          assertEqual "a102 optional" False (Md.optional a102)
          assertEqual "a102 iterable" True (Md.iterable a102)
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_iterable_choice.xsd"
          let key = makeTargetQName "ONIXMessage"
              es = (Md.elements . topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              actual = head es
          assertEqual "can parse unbounded choices(optional)" True (Md.optional actual)
          assertEqual "can parse unbounded choices(iterable)" True (Md.iterable actual)
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_notforsale.xsd"
          let key = makeTargetQName "NotForSale"
              actual = (topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              expected =
                Model
                  { shortname = "notforsale",
                    xmlReferenceName = "NotForSale",
                    typeName = Nothing,
                    kind = Tag,
                    optional = False,
                    iterable = False,
                    elements =
                      [ Model {shortname = "a100", xmlReferenceName = "RightsTerritory", typeName = Just "string", kind = Tag, optional = True, iterable = False, elements = []},
                        Model {shortname = "a000", xmlReferenceName = "RightsCountry", typeName = Just "string", kind = Tag, optional = True, iterable = True, elements = []},
                        Model {shortname = "a200", xmlReferenceName = "ISBN", typeName = Just "string", kind = Tag, optional = False, iterable = False, elements = []},
                        Model {shortname = "a300", xmlReferenceName = "EAN13", typeName = Just "string", kind = Tag, optional = False, iterable = False, elements = []},
                        Model {shortname = "a400", xmlReferenceName = "ProductIdentifier", typeName = Just "string", kind = Tag, optional = False, iterable = True, elements = []},
                        Model {shortname = "a500", xmlReferenceName = "PublisherName", typeName = Just "string", kind = Tag, optional = False, iterable = False, elements = []}
                      ]
                  }

          assertEqual "can drop deplicated field" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_publication_date.xsd"
          let key = makeTargetQName "Product"
              actual = (topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              expected =
                Model
                  { shortname = "product",
                    xmlReferenceName = "Product",
                    typeName = Nothing,
                    kind = Tag,
                    optional = False,
                    iterable = False,
                    elements =
                      [ Model {shortname = "a100", xmlReferenceName = "PublicationDate", typeName = Just "string", kind = Tag, optional = True, iterable = False, elements = []}
                      ]
                  }

          assertEqual "can parse sequence of sequence" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_model_general_attributes_simple.xsd"
          print ""
          print scm
          let key = makeTargetQName "PublishingStatus"
              actual = (topLevelModels scm . unwrap . M.lookup key . schemaElements) scm
              expected =
                Model
                  { shortname = "a000",
                    xmlReferenceName = "PublishingStatus",
                    typeName = Nothing,
                    kind = Tag,
                    optional = False,
                    iterable = False,
                    elements =
                      [ Model {shortname = "a100", xmlReferenceName = "PublicationDate", typeName = Just "string", kind = Tag, optional = True, iterable = False, elements = []}
                      ]
                  }
          assertEqual "can parse generalAttributes" expected actual
      )
  ]
