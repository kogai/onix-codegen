{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Code (Code, CodeType, CodeTypes, code, codeType, codeTypes)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Text.XML as XML
import Text.XML.Cursor
  ( Cursor,
    attribute,
    attributeIs,
    check,
    content,
    element,
    fromDocument,
    hasAttribute,
    ($//),
    (&//),
    (>=>),
  )

type ReferenceName = Text

type ListName = Text

type ReferenceNameAndListName = (ReferenceName, ListName)

name :: String -> XML.Name
name x = XML.Name (pack x) (Just "http://www.w3.org/2001/XMLSchema") (Just "xs")

referenceNames :: Cursor -> [ReferenceNameAndListName]
referenceNames rootOfRef =
  let ys =
        rootOfRef
          $// ( element (name "element")
                  >=> check (hasAttribute "name")
                  &// element (name "simpleContent")
                  &// element (name "extension")
                  >=> check
                    ( \c ->
                        let attrs = attribute "base" c
                         in any (T.isPrefixOf "List") attrs
                    )
              )
      zs =
        map
          ( \c ->
              let listName = attribute "base" c
                  attr =
                    c
                      $// ( element (name "attribute")
                              >=> check (attributeIs "name" "refname")
                              >=> attribute "fixed"
                          )
               in (T.concat attr, T.concat listName)
          )
          ys
   in zs

establishCodes :: Cursor -> [Code]
establishCodes simpleType =
  let enumerations = simpleType $// element (name "enumeration")
   in map
        ( \e ->
            let value = T.concat $ attribute "value" e
                docs = e $// element (name "documentation") &// content
             in code value (head docs) (last docs)
        )
        enumerations

establishCodeType :: ReferenceNameAndListName -> Cursor -> CodeType
establishCodeType (refNm, _listNm) simpleType =
  let docWithSrc = element (name "documentation") >=> check (hasAttribute "source")
      description1 = T.concat $ simpleType $// docWithSrc >=> attribute "source"
      description2 = T.concat $ simpleType $// docWithSrc &// content
      description = T.concat [description1, " ", description2]
   in codeType refNm description $ establishCodes simpleType

findSimpleTypeBy :: Text -> Cursor -> [Cursor]
findSimpleTypeBy listNm = element (name "simpleType") >=> check (attributeIs "name" listNm)

readSchema :: IO CodeTypes
readSchema = do
  xmlCodeLists <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_CodeLists.xsd"
  xmlReference <- XML.readFile XML.def "./2_1_rev03_schema/ONIX_BookProduct_Release2.1_reference.xsd"
  let rootOfCodeLists = fromDocument xmlCodeLists
      rootOfRef = fromDocument xmlReference
      codeLists =
        map
          ( \(refNm, listNm) ->
              let sympleTypes = head $ rootOfCodeLists $// findSimpleTypeBy listNm
               in establishCodeType (refNm, listNm) sympleTypes
          )
          (referenceNames rootOfRef)
  return $ codeTypes codeLists
