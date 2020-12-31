{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Code (code, codeType, codeTypes, CodeTypes, CodeType, Code) where

import Data.Text (Text, pack)
import Data.Vector (Vector, fromList)
import qualified Data.Yaml.Aeson as A
import GHC.Generics
import Text.Mustache

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
