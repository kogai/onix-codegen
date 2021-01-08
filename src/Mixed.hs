{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mixed (Mixed (..), topLevelMixed) where

-- import Data.List (elemIndex, find)
-- import qualified Data.Map as M
-- import Data.Maybe (fromMaybe, mapMaybe)
-- import Data.Text (Text, pack, unpack)
import Data.Text
-- import Data.Vector (Vector, fromList)
-- import Data.Yaml (FromJSON (..), withText)
-- import GHC.Generics (Generic)
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
      [ pack "shortname" ~> shortname,
        pack "xmlReferenceName" ~> xmlReferenceName
      ]

topLevelMixed :: X.Schema -> X.ElementInline -> Mixed
topLevelMixed xsd elm = throw Unimplemented

-- let modelGroup = contentModel elm
--     attributes = contentAttributes elm
--     shortname = unwrap $ findFixedOf "shortname" attributes
--     refname = unwrap $ findFixedOf "refname" attributes
--     elements = case modelGroup of
--       Just mdgrp -> fieldsOfElement xsd mdgrp
--       Nothing -> []
--  in model shortname refname Nothing Tag False False elements

-- readSchema :: IO Models
-- readSchema = do
--   xsd <- X.getSchema "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
--   ( return
--       . models
--       . map (topLevelModels xsd)
--       . collectElements
--     )
--     xsd
