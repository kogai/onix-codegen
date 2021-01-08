{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mixed (Mixed (..), topLevelMixed) where

import Data.Text
import qualified Model as M
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

complexMixed :: X.ElementInline -> Bool
complexMixed
  X.ElementInline
    { X.elementType = X.Inline (X.TypeComplex X.ComplexType {X.complexMixed = mixed})
    } = mixed
complexMixed _ = False

topLevelMixed :: X.Schema -> X.ElementInline -> Maybe Mixed
topLevelMixed _xsd elm =
  let attributes = M.contentAttributes elm
      mixed = complexMixed elm
      xmlReferenceName = unwrap $ M.findFixedOf "refname" attributes
      shortname = unwrap $ M.findFixedOf "shortname" attributes
   in if mixed
        then Just $ Mixed xmlReferenceName shortname
        else Nothing

-- readSchema :: IO Models
-- readSchema = do
--   xsd <- X.getSchema "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
--   ( return
--       . models
--       . map (topLevelModels xsd)
--       . collectElements
--     )
--     xsd
