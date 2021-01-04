{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Code (code, codeType, codeTypes, CodeTypes, CodeType, Code, collectCodes, readSchema, topLevelCodeType) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Vector (Vector, fromList)
import qualified Data.Yaml.Aeson as A
import Debug.Trace
import GHC.Generics (Generic)
import Model (content, contentAttributes, findFixedOf)
import Text.Mustache (ToMustache (..), object, (~>))
import Util
import qualified Xsd as X

data Code = Code
  { value :: Text,
    description :: Text,
    notes :: Text
  }
  deriving (Generic, Show, Eq)

instance A.ToJSON Code

instance A.FromJSON Code

instance ToMustache Code where
  toMustache Code {value, description, notes} =
    object
      [ pack "value" ~> value,
        pack "description" ~> description,
        pack "notes" ~> notes
      ]

code :: Text -> Text -> Text -> Code
code v d n =
  Code
    { value = v,
      description = d,
      notes = n
    }

data CodeType = CodeType
  { xmlReferenceName :: Text,
    description :: Text,
    codes :: Vector Code
  }
  deriving (Generic, Show, Eq)

instance A.ToJSON CodeType

instance A.FromJSON CodeType

instance ToMustache CodeType where
  toMustache CodeType {xmlReferenceName, description, codes} =
    object
      [ pack "xmlReferenceName" ~> xmlReferenceName,
        pack "description" ~> description,
        pack "codes" ~> toMustache codes
      ]

codeType :: Text -> Text -> [Code] -> CodeType
codeType n d cs =
  CodeType
    { xmlReferenceName = n,
      description = d,
      codes = fromList cs
    }

type CodeTypes = Vector CodeType

codeTypes :: [CodeType] -> CodeTypes
codeTypes = fromList

readSchema :: IO CodeTypes
readSchema = do
  xsd <- X.getSchema "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  ( return
      . codeTypes
      . map (topLevelCodeType xsd)
      . collectCodes
    )
    xsd

typeAnnotations :: X.Type -> [X.Annotation]
typeAnnotations ty =
  case ty of
    (X.TypeSimple (X.AtomicType _ annotations)) -> annotations
    (X.TypeSimple (X.ListType _ annotations)) -> annotations
    (X.TypeSimple (X.UnionType _ annotations)) -> annotations
    (X.TypeComplex X.ComplexType {X.complexAnnotations}) -> complexAnnotations

typeConstraints :: X.Type -> [X.Constraint]
typeConstraints ty =
  case ty of
    (X.TypeSimple (X.AtomicType X.SimpleRestriction {X.simpleRestrictionConstraints} _)) -> simpleRestrictionConstraints
    (X.TypeSimple (X.ListType _ _)) -> []
    (X.TypeSimple (X.UnionType _ _)) -> []
    (X.TypeComplex X.ComplexType {X.complexAnnotations = _}) -> []

topLevelCodeType :: X.Schema -> X.Element -> CodeType
topLevelCodeType scm elm =
  let plainContentAttributes = contentAttributes elm
      keyOfType = case content elm of
        Just (X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {X.simpleExtensionBase})) ->
          let qnName = X.qnName simpleExtensionBase
           in if T.isPrefixOf (pack "List") qnName then Just simpleExtensionBase else Nothing
        Just (X.ContentSimple (X.SimpleContentRestriction _)) -> Nothing
        Just (X.ContentPlain X.PlainContent {}) -> Nothing
        Just (X.ContentComplex (X.ComplexContentExtension X.ComplexExtension {})) -> Nothing
        Just (X.ContentComplex (X.ComplexContentRestriction X.ComplexRestriction {})) -> Nothing
        Nothing -> Nothing
      ty = case keyOfType of
        Just key_ ->
          -- NOTE: Codelists does not contain namespaces which refer to `http://www.editeur.org/onix/2.1/reference`
          let key = X.QName Nothing $ X.qnName key_
           in (M.lookup key . X.schemaTypes) scm
        Nothing -> Nothing
      desc = case ty of
        Just t ->
          (T.intercalate (pack ". ") . map (\(X.Documentation x) -> x) . typeAnnotations) t
        Nothing -> pack ""
      codes_ = case ty of
        Just t ->
          let constraints = typeConstraints t
              enums =
                map
                  ( \(X.Enumeration v docs) ->
                      let docs_ = map (\(X.Documentation d) -> d) docs
                       in code v (head docs_) (last docs_)
                  )
                  constraints
           in enums
        Nothing -> []
      refname = unwrap $ findFixedOf "refname" plainContentAttributes
   in codeType refname desc codes_

collectCodes :: X.Schema -> [X.Element]
collectCodes =
  map snd
    . M.toList
    . M.filter
      ( \x -> case content x of
          Just (X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {X.simpleExtensionBase})) ->
            let qnName = X.qnName simpleExtensionBase
             in T.isPrefixOf (pack "List") qnName
          Just (X.ContentSimple (X.SimpleContentRestriction _)) -> False
          Just (X.ContentPlain X.PlainContent {}) -> False
          Just (X.ContentComplex (X.ComplexContentExtension X.ComplexExtension {})) -> False
          Just (X.ContentComplex (X.ComplexContentRestriction X.ComplexRestriction {})) -> False
          Nothing -> False
      )
    . X.schemaElements
