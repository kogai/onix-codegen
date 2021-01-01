{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model (Kind (..), Models, Model, models, model) where

import Data.Text (Text, pack, unpack)
import Data.Vector (Vector, fromList)
import Data.Yaml (FromJSON (..), withText)
import GHC.Generics (Generic)
import Text.Mustache (ToMustache (..), object, (~>))

data Kind
  = Tag
  | Attribute
  deriving (Generic, Show)

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
    elements :: [Model]
  }
  deriving (Generic, Show)

instance FromJSON Model

instance ToMustache Model where
  toMustache Model {shortname, xmlReferenceName, kind, elements, typeName} =
    let typeName_ = case typeName of
          Nothing -> []
          Just t -> [pack "typeName" ~> t]
     in object $
          [ pack "shortname" ~> shortname,
            pack "xmlReferenceName" ~> xmlReferenceName,
            pack "kind" ~> kind,
            pack "elements" ~> elements
          ]
            ++ typeName_

model :: Text -> Text -> Maybe Text -> Kind -> [Model] -> Model
model shortname xmlReferenceName typeName kind elements = Model {shortname, xmlReferenceName, kind, elements, typeName}

type Models = Vector Model

models :: [Model] -> Models
models = fromList
