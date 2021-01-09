{-# LANGUAGE OverloadedStrings #-}

module TestUtils (makeTargetQName) where

import Data.Text (Text, pack, unpack)
import Model (Kind (Tag), dropDuplicate, model, models)
import Test.HUnit (Test (TestCase, TestList), assertEqual)
import Xsd
  ( Namespace (Namespace, fromNamespace),
    QName (QName, qnName, qnNamespace),
  )

makeTargetQName :: Text -> QName
makeTargetQName n =
  QName
    { qnNamespace = Just (Namespace {fromNamespace = "http://www.editeur.org/onix/2.1/reference"}),
      qnName = n
    }
