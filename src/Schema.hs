{-# LANGUAGE OverloadedStrings #-}

module Schema where

-- import Code (code, codeType, codeTypes)

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Debug.Trace (trace)
import Flow
import qualified Text.XML as XML
import Text.XML.Cursor

name :: String -> XML.Name
name x = XML.Name (pack x) (Just "http://www.w3.org/2001/XMLSchema") (Just "xs")

isStartWith :: Text -> Cursor -> Bool
isStartWith s c =
  let attrs = attribute "name" c
   in any (T.isPrefixOf s) attrs

findReferenceName :: Text -> Cursor -> Maybe Text
findReferenceName listName root =
  let xs =
        root
          $// ( element (name "extension")
                  >=> attributeIs "base" listName
                  &// content
              )
      _ys = trace "query=" (root $// element (name "extension"))
   in listToMaybe xs

-- <xs:extension base="List44">
-- 	<xs:attribute name="refname" type="xs:NMTOKEN" fixed="AddresseeIDType"/>
-- 	<xs:attribute name="shortname" type="xs:NMTOKEN" fixed="m380"/>
-- 	<xs:attributeGroup ref="generalAttributes"/>

readSchema :: IO ()
readSchema = do
  xmlCodeLists <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_CodeLists.xsd"
  xmlReference <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  let rootofCodeLists = fromDocument xmlCodeLists
      rootOfRef = fromDocument xmlReference
      query =
        rootofCodeLists
          $// ( element (name "simpleType")
                  >=> check (isStartWith "List")
              )
      codeTypes_ =
        query
          |> map
            ( \c ->
                let listName = c |> attribute "name" |> T.concat
                    description1 =
                      T.concat $
                        c $// element (name "documentation")
                          >=> check (hasAttribute "source")
                          >=> attribute "source"
                    description2 =
                      T.concat $
                        c $// element (name "documentation")
                          >=> check (hasAttribute "source")
                          &// content
                    _description = T.concat [description1, " ", description2]
                 in [fromMaybe "" (findReferenceName listName rootOfRef)]
            )
      ys =
        rootOfRef
          $// ( element (name "extension")
                  >=> check (attributeIs "base" "List44")
                  -- >=> attribute "base"
              )

  -- f = map (attribute "name")
  -- listIDs = query >>= f
  -- cursor $// element "h2"
  --      >=> attributeIs "class" "bar"
  --      >=> precedingSibling
  --      >=> element "h1"
  --      &// content
  print $ length query
  -- print $ map (attribute "name") query
  print ys
  print $ length ys
  print codeTypes_
  print $ length codeTypes_
