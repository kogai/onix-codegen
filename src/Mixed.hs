{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mixed (Mixed (..), topLevelMixed, collectElements, collectTypes, typeToMixed) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Model as Md
import Text.Mustache (ToMustache (..), object, (~>))
import Util
import qualified Xsd as X

data Mixed = Mixed
  { xmlReferenceName :: Text,
    shortname :: Text
  }
  deriving (Show, Eq)

instance ToMustache Mixed where
  toMustache Mixed {shortname, xmlReferenceName} =
    object
      [ "shortname" ~> shortname,
        "xmlReferenceName" ~> xmlReferenceName
      ]

complexMixed :: X.ElementInline -> Bool
complexMixed
  X.ElementInline
    { X.elementType = X.Inline (X.TypeComplex X.ComplexType {X.complexMixed = mixed})
    } = mixed
complexMixed _ = False

topLevelMixed :: X.Schema -> X.ElementInline -> Maybe Mixed
topLevelMixed _xsd elm =
  let attributes = Md.contentAttributes elm
      mixed = complexMixed elm
      xmlReferenceName = Md.findFixedOf "refname" attributes
      shortname = Md.findFixedOf "shortname" attributes
   in case (xmlReferenceName, shortname, mixed) of
        (Just xmlReferenceName_, Just shortname_, True) -> Just $ Mixed xmlReferenceName_ shortname_
        _ -> Nothing

typeToMixed :: X.Schema -> X.Type -> Mixed
typeToMixed _scm (X.TypeComplex X.ComplexType {X.complexName}) =
  Mixed {xmlReferenceName = sanitizeName $ maybe "" X.qnName complexName, shortname = ""}
typeToMixed _scm t =
  unimplemented [show t]

collectTypes :: X.Schema -> [X.Type]
collectTypes =
  map snd
    . M.toList
    . M.filter
      ( \case
          X.TypeComplex X.ComplexType {X.complexMixed = True} -> True
          _ -> False
      )
    . X.schemaTypes

collectElements :: X.Schema -> [X.ElementInline]
collectElements =
  map snd
    . M.toList
    . M.filter
      ( \case
          X.ElementInline
            { X.elementType = X.Inline (X.TypeComplex X.ComplexType {X.complexContent = X.ContentPlain _, X.complexMixed = True})
            } -> True
          _ -> False
      )
    . X.schemaElements

instance GenSchema [Mixed] where
  readSchema scm =
    fromElements ++ fromTypes
    where
      fromElements = mapMaybe (topLevelMixed scm) . collectElements $ scm
      fromTypes = map (typeToMixed scm) . collectTypes $ scm
