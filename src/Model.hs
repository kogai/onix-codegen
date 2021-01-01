{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model (Kind (..), Models, Model, models, model, readSchema) where

import Data.List (elemIndex)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Vector (Vector, fromList)
import Data.Yaml (FromJSON (..), withText)
import Flow
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
  deriving (Generic, Show, Eq)

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
    optional :: Bool,
    elements :: [Model]
  }
  deriving (Generic, Show, Eq)

instance FromJSON Model

instance ToMustache Model where
  toMustache Model {shortname, xmlReferenceName, kind, elements, typeName, optional} =
    let typeName_ = case typeName of
          Nothing -> []
          Just t -> [pack "typeName" ~> t]
        -- optional_ = if optional then [pack "optional" ~> optional] else []
        optional_ = ([pack "optional" ~> optional | optional])
     in object $
          [ pack "shortname" ~> shortname,
            pack "xmlReferenceName" ~> xmlReferenceName,
            pack "kind" ~> kind,
            pack "elements" ~> elements
          ]
            ++ typeName_
            ++ optional_

model :: Text -> Text -> Maybe Text -> Kind -> Bool -> [Model] -> Model
model shortname xmlReferenceName typeName kind optional elements =
  Model
    { shortname,
      xmlReferenceName,
      kind,
      elements,
      typeName,
      optional
    }

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

findFixedOf :: String -> Cursor -> [Text]
findFixedOf s =
  element (nameNs "attribute")
    >=> check (attributeIs (name "name") (pack s))
    >=> attribute (name "fixed")

modelOfElement :: Cursor -> Cursor -> Model
modelOfElement docOfRef c =
  let ref = T.concat $ attribute (name "ref") c
      shrtnm =
        docOfRef $// element (nameNs "element") >=> check (attributeIs (name "name") ref)
          |> map (\s -> s $// findFixedOf "shortname")
          |> concat
          |> T.concat
   in model shrtnm ref (Just ref) Tag False []

-- TODO: Branching by language
-- Since the Go language does not have a sum type, all choices should be optional.
modelsOfElement :: Cursor -> [Cursor] -> [Model]
modelsOfElement docOfRef elements =
  elements
    |> map (modelOfElement docOfRef)
    |> foldr
      ( \x acc ->
          case elemIndex x acc of
            Just i -> x {optional = True} : drop i acc
            Nothing -> x : acc
      )
      []

tagElement :: Cursor -> Cursor -> Model
tagElement docOfRef csr =
  let xmlReferenceName = T.concat $ csr $// findFixedOf "refname"
      shortname = T.concat $ csr $// findFixedOf "shortname"
      elements = csr $// element (nameNs "sequence") &// element (nameNs "element")
   in model shortname xmlReferenceName Nothing Tag False (modelsOfElement docOfRef elements)

readSchema :: IO Models
readSchema = do
  xmlCodeLists <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_CodeLists.xsd"
  xmlReference <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  let _docOfCodeLists = fromDocument xmlCodeLists
      docOfRef = fromDocument xmlReference
      targetElements = collectElements docOfRef
  return $ models $ map (tagElement docOfRef) targetElements
