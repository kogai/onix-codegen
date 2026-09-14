{-# LANGUAGE OverloadedStrings #-}

module TestMixed (tests) where

import qualified Data.Map as M
import Mixed
import Test.HUnit (Test (TestCase), assertEqual)
import Util (unwrap)
import Xsd

tests :: [Test]
tests =
  [ TestCase
      ( do
          scm <- getSchema "./fixtures/test_mixed_html.xsd"
          let key =
                QName
                  { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
                    qnName = "Annotation"
                  }

          let actual = (topLevelMixed scm . unwrap . M.lookup key . schemaElements) scm
              expected = Just $ Mixed "Annotation" "d100"
          assertEqual "can parse mixed of html string" expected actual
      ),
    TestCase
      ( do
          scm <- getSchema "./fixtures/test_mixed_flow.xsd"
          let actual = (map (typeToMixed scm) . collectTypes) scm
              expected = [Mixed {xmlReferenceName = "Flow", shortname = ""}, Mixed {xmlReferenceName = "Inline", shortname = ""}]
          assertEqual "can parse mixed of flow" expected actual
      )
  ]
