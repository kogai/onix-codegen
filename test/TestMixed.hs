{-# LANGUAGE OverloadedStrings #-}

module TestModel (tests) where

import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Mixed
import qualified Model as Md
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Text.XML (def, parseText, readFile)
import Util
import Xsd

tests =
  [ TestCase
      ( do
          scm <- getSchema "./fixtures/test_mixed_html.xsd"
          let key =
                QName
                  { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
                    qnName = "Annotation"
                  }
              actual = (topLevelMixed scm . unwrap . M.lookup key . schemaElements) scm
              expected = Mixed "Annotation" "d100"
          assertEqual "can parse mixed of html string" expected actual
      )
  ]
