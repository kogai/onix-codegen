{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mixed (Mixed (..), topLevelMixed, readSchema) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Model as Md
import Text.Mustache (ToMustache (..), object, (~>))
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

readSchema :: IO [Mixed]
readSchema = do
  xsd <- X.getSchema "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  ( return
      . mapMaybe (topLevelMixed xsd)
      . collectElements
    )
    xsd
