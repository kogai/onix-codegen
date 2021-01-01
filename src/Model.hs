{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model (Kind (..), Models, Model, models, model, readSchema) where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Vector (Vector, fromList)
import Data.Yaml (FromJSON (..), withText)
import GHC.Generics (Generic)
import Text.Mustache (ToMustache (..), object, (~>))
import qualified Text.XML as XML
import Text.XML.Cursor

nameNs :: String -> XML.Name
nameNs x = XML.Name (pack x) (Just $ pack "http://www.w3.org/2001/XMLSchema") (Just $ pack "xs")

name :: String -> XML.Name
name x = XML.Name (pack x) Nothing Nothing

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

collectElements :: Cursor -> [Cursor]
collectElements docOfRef =
  docOfRef
    $// ( element (nameNs "element")
            >=> check (hasAttribute $ name "name")
            >=> check
              ( \csr ->
                  let seqInChildren = csr $// element (nameNs "sequence")
                   in not $ null seqInChildren
              )
        )

tagElement :: Cursor -> Model
tagElement csr =
  let findFixedOf s =
        element (nameNs "attribute")
          >=> check (attributeIs (name "name") (pack s))
          >=> attribute (name "fixed")
      xmlReferenceName = T.concat $ csr $// findFixedOf "refname"
      shortname = T.concat $ csr $// findFixedOf "shortname"
   in model shortname xmlReferenceName Nothing Tag []

readSchema :: IO Models
readSchema = do
  xmlCodeLists <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_CodeLists.xsd"
  xmlReference <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  let _docOfCodeLists = fromDocument xmlCodeLists
      docOfRef = fromDocument xmlReference
      targetElements = collectElements docOfRef
  -- models_ =
  --   models
  --     [ model
  --         ""
  --         "Onix"
  --         Nothing
  --         Tag
  --         [ model "ONIXmessage" "XMLName" (Just "xml.Name") Tag [],
  --           model "header" "Header" (Just "Header") Tag [],
  --           model "product" "Products" (Just "[]Product") Tag []
  --         ],
  --       model
  --         "header"
  --         "Header"
  --         Nothing
  --         Tag
  --         [ model "m174" "FromCompany" (Just "string") Tag [],
  --           model "m182" "SentDate" (Just "string") Tag [],
  --           model "m184" "DefaultLanguageOfText" (Just "string") Tag [],
  --           model "m185" "DefaultPriceTypeCode" (Just "string") Tag [],
  --           model "m186" "DefaultCurrencyCode" (Just "string") Tag []
  --         ]
  --     ]
  print $ length targetElements
  print $ head targetElements
  return $ models $ map tagElement targetElements
