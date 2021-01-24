{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Kind (..),
    Models,
    Model (..),
    models,
    model,
    dropDuplicate,
    typeToText,
    content,
    findFixedOf,
    contentAttributes,
    collectElements,
    topLevelModels,
    topLevelAttribute,
    fieldsOfAttribute,
  )
where

import Data.List (find, findIndex)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, toTitle, unpack)
import qualified Data.Text as T
import Data.Vector (Vector, fromList)
import Data.Yaml (FromJSON (..), withText)
import GHC.Generics (Generic)
import Text.Mustache (ToMustache (..), object, (~>))
import Util
import qualified Xsd as X

data Kind
  = Tag
  | Attribute
  deriving (Generic, Show, Eq, Ord)

instance FromJSON Kind where
  parseJSON = withText "kind" $ \t -> case unpack t of
    "tag" -> return Tag
    "attribute" -> return Tag
    _ -> fail "string is not one of known enum values"

instance ToMustache Kind where
  toMustache Tag = toMustache ("tag" :: String)
  toMustache Attribute = toMustache ("attribute" :: String)

data Model = Model
  { shortname :: Text,
    xmlReferenceName :: Text,
    typeName :: Maybe Text,
    kind :: Kind,
    optional :: Bool,
    iterable :: Bool,
    elements :: [Model]
  }
  deriving (Generic, Show, Eq, Ord)

instance FromJSON Model

instance ToMustache Model where
  toMustache Model {shortname, xmlReferenceName, kind, elements, typeName, optional, iterable} =
    let typeName_ = case typeName of
          Nothing -> []
          Just t -> [pack "typeName" ~> t]
     in object $
          [ "shortname" ~> shortname,
            "xmlReferenceName" ~> xmlReferenceName,
            "is_tag" ~> (kind == Tag),
            "optional" ~> optional,
            "iterable" ~> iterable,
            "elements" ~> elements
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

collectElements :: X.Schema -> [X.ElementInline]
collectElements =
  map snd
    . M.toList
    . M.filter
      ( \case
          X.ElementInline
            { X.elementType = X.Inline (X.TypeComplex X.ComplexType {X.complexContent = X.ContentPlain _, X.complexMixed = False}),
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
          X.InlineAttribute X.AttributeInline {X.attributeInlineFixed = Just name} -> Just name
          X.InlineAttribute X.AttributeInline {X.attributeInlineType = X.Inline ty} ->
            case ty of
              ((X.AtomicType X.SimpleRestriction {X.simpleRestrictionConstraints = [X.Enumeration ty' _]} [])) -> Just ty'
              _ -> Nothing
          _ -> Nothing
      )
    . find
      ( \case
          X.InlineAttribute X.AttributeInline {X.attributeInlineName = X.QName {X.qnName}} -> unpack qnName == s
          _ -> False
      )

-- TODO: Use Util.uniq instead
dropDuplicate :: [Model] -> [Model]
dropDuplicate =
  foldl
    ( \acc x ->
        case findIndex
          ( \y ->
              shortname x == shortname y
                && xmlReferenceName x == xmlReferenceName y
                && elements x == elements y
          )
          acc of
          Just i ->
            let (xs, ys) = splitAt i acc
             in xs ++ [x {optional = True}] ++ tail ys
          Nothing -> acc ++ [x]
    )
    []

typeToText :: X.Type -> Text
typeToText (X.TypeSimple (X.AtomicType X.SimpleRestriction {X.simpleRestrictionBase} _annotations)) =
  case simpleRestrictionBase of
    X.Ref n -> X.qnName n
    X.Inline x -> unreachable ["TypeSimple.AtomicType expect base type name", show x]
typeToText (X.TypeSimple (X.ListType ty annotations)) = unimplemented ["ListType", show ty, show annotations]
typeToText (X.TypeSimple (X.UnionType ty annotations)) = unimplemented ["UnionType", show ty, show annotations]
typeToText (X.TypeComplex X.ComplexType {X.complexContent}) = case complexContent of
  X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {X.simpleExtensionBase, X.simpleExtensionAttributes}) ->
    let qnName = X.qnName simpleExtensionBase
     in if T.isPrefixOf "List" qnName
          then unwrap $ findFixedOf "refname" simpleExtensionAttributes
          else case qnName of
            "NonEmptyString" -> configurableType
            _ -> qnName
  X.ContentPlain (X.PlainContent _mdg annotations) -> fromMaybe configurableType $ findFixedOf "refname" annotations
  _ -> throw Unimplemented

isIterable :: X.Occurs -> Bool
isIterable (X.Occurs (_, m)) = isIterable' m

isIterable' :: X.MaxOccurs -> Bool
isIterable' (X.MaxOccurs 1) = False
isIterable' (X.MaxOccurs _) = True
isIterable' X.MaxOccursUnbound = True

isOptional :: X.Occurs -> Bool
isOptional (X.Occurs (m, _)) = isOptional' m

isOptional' :: (Eq a, Num a) => a -> Bool
isOptional' m = m == 0

extendOccurs :: X.Occurs -> Model -> Model
extendOccurs (X.Occurs (1, X.MaxOccurs 1)) x = x
extendOccurs (X.Occurs (m, X.MaxOccurs 1)) x = x {optional = isOptional' m}
extendOccurs (X.Occurs (1, m)) x = x {iterable = isIterable' m}
extendOccurs occurs x = x {iterable = isIterable occurs, optional = isOptional occurs}

elementToModel :: X.Schema -> X.ElementInline -> Model
elementToModel docOfRef x =
  let shortname = (unwrap . findFixedOf "shortname" . contentAttributes) x
      refname = (unwrap . findFixedOf "refname" . contentAttributes) x
      iterable_ = isIterable . X.elementOccurs $ x
      optional_ = X.elementNillable x || (isOptional . X.elementOccurs $ x)
      ty = case X.elementType x of
        X.Ref key -> (M.lookup key . X.schemaTypes) docOfRef
        X.Inline val -> Just val
   in model shortname refname (fmap typeToText ty) Tag optional_ iterable_ []

modelByKey :: X.Schema -> X.QName -> Maybe Model
modelByKey docOfRef key =
  let elm = (M.lookup key . X.schemaElements) docOfRef
   in case elm of
        Just e -> Just $ elementToModel docOfRef e
        Nothing -> Nothing

modelByRef :: X.Schema -> X.ElementRef -> Maybe Model
modelByRef docOfRef ref =
  let key = X.elementRefName ref
      occurs = X.elementRefOccurs ref
      elm = (M.lookup key . X.schemaElements) docOfRef
   in case elm of
        Just e -> Just $ elementToModel docOfRef (e {X.elementOccurs = occurs})
        Nothing -> Nothing

makeOptional :: Model -> Model
makeOptional x = x {optional = True}

fieldsOfElementOfChoiceInChild :: X.Schema -> [X.RefOr X.ChoiceInChild] -> [Model]
fieldsOfElementOfChoiceInChild docOfRef =
  concatMap
    ( X.refOr
        (\key -> [unwrap $ modelByKey docOfRef key])
        ( \case
            (X.ElementOfChoice occurs es) ->
              map (extendOccurs occurs)
                . mapMaybe
                  ( \case
                      X.RefElement ref -> modelByRef docOfRef ref
                      X.InlineElement value -> (Just . elementToModel docOfRef) value
                  )
                $ es
            (X.SequenceOfChoice occurs ss) ->
              map (extendOccurs occurs)
                . fieldsOfElement docOfRef
                . X.Sequence
                $ ss
            (X.ChoiceInChoice occurs ss) ->
              map (makeOptional . extendOccurs occurs)
                . fieldsOfElementOfChoiceInChild docOfRef
                $ ss
        )
    )

-- TODO: Branching by language
-- Since the Go language does not have a sum type, all choices should be optional.
fieldsOfElement :: X.Schema -> X.ModelGroup -> [Model]
fieldsOfElement docOfRef (X.Sequence xs) =
  dropDuplicate
    . concatMap
      ( \case
          (X.Ref key) ->
            let plainContentModel = (contentModel . unwrap . M.lookup key . X.schemaElements) docOfRef
             in case plainContentModel of
                  Just mdgrp -> fieldsOfElement docOfRef mdgrp
                  Nothing -> []
          (X.Inline (X.ElementOfSequence occurs ys)) ->
            map (extendOccurs occurs)
              . mapMaybe
                ( \case
                    X.RefElement ref -> modelByRef docOfRef ref
                    X.InlineElement value -> (Just . elementToModel docOfRef) value
                )
              $ ys
          (X.Inline (X.ChoiceOfSequence occurs ys)) ->
            map (makeOptional . extendOccurs occurs)
              . fieldsOfElementOfChoiceInChild docOfRef
              $ ys
          (X.Inline (X.SequenceInSequence occurs ys)) ->
            map (extendOccurs occurs)
              . fieldsOfElement docOfRef
              . X.Sequence
              $ ys
      )
    $ xs
fieldsOfElement docOfRef (X.Choice xs) = (dropDuplicate . map makeOptional . fieldsOfElementOfChoiceInChild docOfRef) xs
fieldsOfElement _docOfRef (X.All _xs) = []

content :: X.ElementInline -> Maybe X.Content
content
  X.ElementInline
    { X.elementType =
        X.Inline
          ( X.TypeComplex
              X.ComplexType
                { X.complexContent
                }
            )
    } = Just complexContent
content _ = Nothing

contentAttributes :: X.ElementInline -> [X.Attribute]
contentAttributes el =
  case content el of
    Just (X.ContentComplex (X.ComplexContentExtension X.ComplexExtension {X.complexExtensionAttributes})) -> complexExtensionAttributes
    Just (X.ContentComplex (X.ComplexContentRestriction X.ComplexRestriction {X.complexRestrictionAttributes})) -> complexRestrictionAttributes
    Just (X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {X.simpleExtensionAttributes})) -> simpleExtensionAttributes
    Just (X.ContentSimple (X.SimpleContentRestriction _)) -> []
    Just (X.ContentPlain X.PlainContent {X.plainContentAttributes}) -> plainContentAttributes
    Nothing -> []

contentModel :: X.ElementInline -> Maybe X.ModelGroup
contentModel el =
  case content el of
    Just (X.ContentComplex (X.ComplexContentExtension X.ComplexExtension {X.complexExtensionModel})) -> complexExtensionModel
    Just (X.ContentComplex (X.ComplexContentRestriction X.ComplexRestriction {X.complexRestrictionModel})) -> complexRestrictionModel
    Just (X.ContentSimple (X.SimpleContentExtension X.SimpleExtension {})) -> Nothing
    Just (X.ContentSimple (X.SimpleContentRestriction _)) -> Nothing
    Just (X.ContentPlain X.PlainContent {X.plainContentModel}) -> plainContentModel
    Nothing -> Nothing

fieldsOfAttribute :: X.Schema -> X.Attribute -> [Model]
fieldsOfAttribute scm (X.AttributeGroupRef key) =
  case value of
    Just v -> fieldsOfAttribute scm v
    Nothing -> []
  where
    value = (M.lookup key . X.schemaAttributes) scm
fieldsOfAttribute scm (X.AttributeGroupInline _ as) = concatMap (fieldsOfAttribute scm) as
fieldsOfAttribute _scm (X.RefAttribute _x) = []
fieldsOfAttribute _scm (X.InlineAttribute X.AttributeInline {X.attributeInlineName = name, X.attributeInlineType, X.attributeInlineUse}) =
  [ Model
      { shortname = X.qnName name,
        xmlReferenceName = xmlReferenceName_,
        typeName = Just $ typeNameToReferenceName xmlReferenceName_ ty,
        kind = Attribute,
        optional = attributeInlineUse /= X.Required,
        iterable = False,
        elements = []
      }
  ]
  where
    ty = case attributeInlineType of
      X.Ref n -> X.qnName n
      X.Inline t -> typeToText . X.TypeSimple $ t
    xmlReferenceName_ = toTitle $ X.qnName name

topLevelAttribute :: X.Attribute -> Bool
topLevelAttribute (X.AttributeGroupRef _) = True
topLevelAttribute (X.AttributeGroupInline _ _) = False
topLevelAttribute (X.RefAttribute _) = False
topLevelAttribute (X.InlineAttribute _) = False

topLevelModels :: X.Schema -> X.ElementInline -> Model
topLevelModels xsd elm =
  let modelGroup = contentModel elm
      attributes = contentAttributes elm
      shortname = unwrap $ findFixedOf "shortname" attributes
      refname = unwrap $ findFixedOf "refname" attributes
      attributes' =
        concatMap (fieldsOfAttribute xsd)
          . filter topLevelAttribute
          $ attributes
      elements = case modelGroup of
        Just mdgrp -> fieldsOfElement xsd mdgrp
        Nothing -> []
   in model shortname refname Nothing Tag False False (elements ++ attributes')

instance GenSchema Models where
  readSchema xsd =
    models
      . map (topLevelModels xsd)
      . collectElements
      $ xsd
