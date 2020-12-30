{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Code (Code (..), code) where

-- import Control.Exception (Exception, throw)
-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HM
import Data.Text
-- import Data.Typeable (Typeable)
-- import Data.Vector (Vector)
import Data.Yaml (FromJSON (..))
-- import qualified Data.Yaml as Y
import qualified Data.Yaml.Aeson as A
import GHC.Generics
import Text.Mustache

-- import qualified Text.Mustache.Types as MT

data Code = Code {value :: Text, description :: Text, notes :: Text} deriving (Generic, Show)

instance A.ToJSON Code

instance FromJSON Code

instance ToMustache Code where
  toMustache Code {value, description, notes} =
    object
      [ pack "value" ~> value,
        pack "description" ~> description,
        pack "notes" ~> notes
      ]

code :: String -> String -> String -> Code
code v d n = Code {value = pack v, description = pack d, notes = pack n}
