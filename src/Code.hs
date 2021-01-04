{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Code (code, codeType, codeTypes, CodeTypes, CodeType, Code, collectCodes, readSchema) where

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
  deriving (Generic, Show)

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
  deriving (Generic, Show)

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
