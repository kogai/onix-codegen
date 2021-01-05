{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model
  ( Kind (..),
    Models,
    Model,
    models,
    model,
    readSchema,
    dropDuplicate,
    typeToText,
    content,
    findFixedOf,
    contentAttributes,
    topLevelModels,
  )
where

import Data.List (elemIndex, find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Vector (Vector, fromList)
import Data.Yaml (FromJSON (..), withText)
import Debug.Trace
import GHC.Generics (Generic)
import Text.Mustache (ToMustache (..), object, (~>))
import Util
import qualified Xsd as X

data Kind
  = Tag
  | Attribute
  deriving (Generic, Show, Eq)

instance FromJSON Kind where
  parseJSON = withText "kind" $ \t -> case unpack t of
    "tag" -> return Tag
    "attribute" -> return Tag
    _ -> fail "string is not one of known enum values"

instance ToMustache Kind where
  toMustache Tag = toMustache "tag"
  toMustache Attribute = toMustache "attribute"

data Model = Model
  { shortname :: Text,
    xmlReferenceName :: Text,
    typeName :: Maybe Text,
    kind :: Kind,
    optional :: Bool,
    iterable :: Bool,
    elements :: [Model]
  }
  deriving (Generic, Show, Eq)

instance FromJSON Model

instance ToMustache Model where
  toMustache Model {shortname, xmlReferenceName, kind, elements, typeName, optional, iterable} =
    let typeName_ = case typeName of
          Nothing -> []
          Just t -> [pack "typeName" ~> t]
     in object $
          [ pack "shortname" ~> shortname,
            pack "xmlReferenceName" ~> xmlReferenceName,
            pack "kind" ~> kind,
            pack "optional" ~> optional,
            pack "iterable" ~> iterable,
            pack "elements" ~> elements
          ]
            ++ typeName_

model :: Text -> Text -> Maybe Text -> Kind -> Bool -> Bool -> [Model] -> Model
model shortname xmlReferenceName typeName kind optional iterable elements =
  Model
    { shortname,
      xmlReferenceName,
      kind,
      elements,
      typeName,
      optional,
      iterable
    }

type Models = Vector Model

models :: [Model] -> Models
models = fromList

collectElements :: X.Schema -> [X.Element]
collectElements =
  map snd
    . M.toList
    . M.filter
      ( \case
          X.Element
            { X.elementType = X.Inline (X.TypeComplex X.ComplexType {X.complexContent = X.ContentPlain _}),
              X.elementName = X.QName {X.qnNamespace = Just _}
            } -> True
          _ -> False
      )
    . X.schemaElements

configurableType :: Text
configurableType = pack "string"

findFixedOf :: String -> [X.Attribute] -> Maybe Text
findFixedOf s =
  ( \case
      Just (Just x) -> Just x
      Just Nothing -> Nothing
      Nothing -> Nothing
  )
    . fmap
      ( \case
          X.InlineAttribute X.AttributeInline {X.attributeInlineFixed} -> attributeInlineFixed
          _ -> Nothing
      )
    . find
      ( \case
          X.InlineAttribute X.AttributeInline {X.attributeInlineName = X.QName {X.qnName}} -> unpack qnName == s
          _ -> False
      )

dropDuplicate :: [Model] -> [Model]
dropDuplicate =
  foldl
    ( \acc x ->
        case elemIndex x acc of
          Just i ->
            let (xs, ys) = splitAt i acc
             in xs ++ [x {optional = True}] ++ tail ys
          Nothing -> acc ++ [x]
    )
    []

typeToText :: X.Type -> Text
typeToText (X.TypeSimple (X.AtomicType X.SimpleRestriction {X.simpleRestrictionBase} [])) =
  X.refOr (\X.QName {X.qnName} -> qnName) (throw Unreachable) simpleRestrictionBase
typeToText (X.TypeSimple (X.AtomicType _ty _annotations)) = throw Unimplemented
typeToText (X.TypeSimple (X.ListType _ty _annotations)) = throw Unimplemented
typeToText (X.TypeSimple (X.UnionType _ty _annotations)) = throw Unimplemented
typeToText (X.TypeComplex X.ComplexType {X.complexContent}) = case complexContent of
  X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {X.simpleExtensionBase, X.simpleExtensionAttributes}) ->
    let qnName = X.qnName simpleExtensionBase
     in if T.isPrefixOf (pack "List") qnName
          then unwrap $ findFixedOf "refname" simpleExtensionAttributes
          else case unpack qnName of
            "NonEmptyString" -> configurableType
            _ -> qnName
  X.ContentPlain (X.PlainContent _mdg annotations) -> fromMaybe configurableType $ findFixedOf "refname" annotations
  _ -> throw Unimplemented

elementToModel :: X.Schema -> X.Element -> Model
elementToModel docOfRef x =
  let shortname = (unwrap . findFixedOf "shortname" . contentAttributes) x
      refname = (unwrap . findFixedOf "refname" . contentAttributes) x
      iterable = case X.elementOccurs x of
        (_, X.MaxOccurs 1) -> False
        (_, X.MaxOccurs _) -> True
        (_, X.MaxOccursUnbound) -> True
      ty = case X.elementType x of
        X.Ref key -> (M.lookup key . X.schemaTypes) docOfRef
        X.Inline val -> Just val
   in model shortname refname (fmap typeToText ty) Tag (X.elementNillable x) iterable []

modelByKey :: X.Schema -> X.QName -> Model
modelByKey docOfRef key = (elementToModel docOfRef . unwrap . M.lookup key . X.schemaElements) docOfRef

makeOptional :: Model -> Model
makeOptional x = x {optional = True}

fieldsOfElementOfChoiceInChild :: X.Schema -> [X.RefOr X.ChoiceInChild] -> [Model]
fieldsOfElementOfChoiceInChild docOfRef =
  concatMap
    ( X.refOr
        (\key -> [modelByKey docOfRef key])
        ( \case
            (X.ElementOfChoice es) ->
              map
                ( \case
                    X.Ref key -> (elementToModel docOfRef . unwrap . M.lookup key . X.schemaElements) docOfRef
                    X.Inline value -> elementToModel docOfRef value
                )
                es
            (X.SequenceOfChoice ss) -> fieldsOfElement docOfRef $ X.Sequence ss
        )
    )

-- TODO: Branching by language
-- Since the Go language does not have a sum type, all choices should be optional.
fieldsOfElement :: X.Schema -> X.ModelGroup -> [Model]
fieldsOfElement docOfRef (X.Sequence xs) =
  concatMap
    ( \case
        (X.Ref key) ->
          let plainContentModel = (contentModel . unwrap . M.lookup key . X.schemaElements) docOfRef
           in case plainContentModel of
                Just mdgrp -> fieldsOfElement docOfRef mdgrp
                Nothing -> []
        (X.Inline (X.ElementOfSequence ys)) ->
          map (X.refOr (modelByKey docOfRef) (elementToModel docOfRef)) ys
        (X.Inline (X.ChoiceOfSequence ys)) -> fieldsOfElementOfChoiceInChild docOfRef ys
    )
    xs
fieldsOfElement docOfRef (X.Choice xs) = (dropDuplicate . map makeOptional . fieldsOfElementOfChoiceInChild docOfRef) xs
fieldsOfElement _docOfRef (X.All _xs) = []

content :: X.Element -> Maybe X.Content
content
  X.Element
    { X.elementType =
        X.Inline
          ( X.TypeComplex
              X.ComplexType
                { X.complexContent
                }
            )
    } = Just complexContent
content _ = Nothing

contentAttributes :: X.Element -> [X.Attribute]
contentAttributes el =
  case content el of
    Just (X.ContentComplex (X.ComplexContentExtension X.ComplexExtension {X.complexExtensionAttributes})) -> complexExtensionAttributes
    Just (X.ContentComplex (X.ComplexContentRestriction X.ComplexRestriction {X.complexRestrictionAttributes})) -> complexRestrictionAttributes
    Just (X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {X.simpleExtensionAttributes})) -> simpleExtensionAttributes
    Just (X.ContentSimple (X.SimpleContentRestriction _)) -> []
    Just (X.ContentPlain X.PlainContent {X.plainContentAttributes}) -> plainContentAttributes
    Nothing -> []

contentModel :: X.Element -> Maybe X.ModelGroup
contentModel el =
  case content el of
    Just (X.ContentComplex (X.ComplexContentExtension X.ComplexExtension {X.complexExtensionModel})) -> complexExtensionModel
    Just (X.ContentComplex (X.ComplexContentRestriction X.ComplexRestriction {X.complexRestrictionModel})) -> complexRestrictionModel
    Just (X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {})) -> Nothing
    Just (X.ContentSimple (X.SimpleContentRestriction _)) -> Nothing
    Just (X.ContentPlain X.PlainContent {X.plainContentModel}) -> plainContentModel
    Nothing -> Nothing

topLevelModels :: X.Schema -> X.Element -> Model
topLevelModels xsd elm =
  let modelGroup = contentModel elm
      attributes = contentAttributes elm
      shortname = unwrap $ findFixedOf "shortname" attributes
      refname = unwrap $ findFixedOf "refname" attributes
      elements = case modelGroup of
        Just mdgrp -> fieldsOfElement xsd mdgrp
        Nothing -> []
   in model shortname refname Nothing Tag False False elements

readSchema :: IO Models
readSchema = do
  xsd <- X.getSchema "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  ( return
      . models
      . map (topLevelModels xsd)
      . collectElements
    )
    xsd
