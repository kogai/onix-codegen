{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( render,
    compile,
    Language (..),
  )
where

import qualified Code as C
import Control.Exception (Exception, throw)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Yaml (FromJSON (..))
import qualified Data.Yaml as Y
-- import qualified Data.Yaml.Aeson as A
import GHC.Generics
import Text.Mustache
import qualified Text.Mustache.Types as MT

data Language
  = Go
  | TypeScript

data Renderer
  = Model
  | Code
  | Decoder
  | Reader
  deriving (Show)

-- instance Show Renderer =>
data Empty
  = Unimplemented
  | Unreachable
  deriving (Show, Typeable)

instance Exception Empty

data SchemaKind
  = Tag
  | Attribute
  deriving (Generic, Show)

instance FromJSON SchemaKind where
  parseJSON = Y.withText "kind" $ \t -> case unpack t of
    "tag" -> return Tag
    "attribute" -> return Tag
    _ -> fail "string is not one of known enum values"

instance ToMustache SchemaKind where
  toMustache Tag = toMustache "tag"
  toMustache Attribute = toMustache "attribute"

data SchemaBody = SchemaBody
  { key :: Text,
    xmlReferenceName :: Text,
    description :: Text,
    kind :: SchemaKind,
    -- TODO: Should be injectable by Language respectively.
    model :: Text
  }
  deriving (Generic, Show)

instance FromJSON SchemaBody

instance ToMustache SchemaBody where
  toMustache SchemaBody {key, xmlReferenceName, description, model, kind} =
    object
      [ pack "key" ~> key,
        pack "xmlReferenceName" ~> xmlReferenceName,
        pack "description" ~> description,
        pack "kind" ~> kind,
        pack "model" ~> model
      ]

data SchemaNode
  = Leaf SchemaBody
  | Node
      { children :: Vector SchemaNode,
        body :: SchemaBody
      }
  deriving (Generic, Show)

instance FromJSON SchemaNode

instance ToMustache SchemaNode where
  toMustache (Leaf s) = toMustache s
  toMustache Node {children, body} =
    let cdr = (MT.Array . fmap toMustache) children
     in toMustache $ HM.fromList [("body", toMustache body), ("children", cdr)]

type Schema = HashMap Text SchemaNode

-- type Schema = Vector SchemaNode

-- - Value: "01"
--   Description: Proprietary
--   Notes: For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers

compile :: Renderer -> Language -> IO (Maybe String)
compile _ TypeScript = throw Unimplemented
compile r Go = do
  let searchSpace = [".", "src"]
      templateName = case r of
        Model -> Just "model"
        Decoder -> Nothing
        Reader -> Just "reader"
        Code -> Just "code"

  case templateName of
    Nothing -> return Nothing
    Just n -> do
      let name = n ++ ".mustache"
      compiled <- automaticCompile searchSpace name
      -- TODO: Use Either
      yml <- Y.decodeFileThrow "./src/schema.yml"
      codeYml <- Y.decodeFileThrow "./src/code.yml"
      print (yml :: Schema)
      let codeTypes = (codeYml :: C.CodeTypes)
      print codeTypes
      -- C.codeTypes
      --   [ C.codeType
      --       "ProductIDType"
      --       "Product identifier type code, List 5"
      --       [ C.code "01" "Proprietary" "For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers",
      --         C.code "02" "ISBN-10" "International Standard Book Number, pre-2007, unhyphenated (10 characters) – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2007 – when ISBN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct GTIN-13 / ISBN-13) For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers",
      --         C.code "03" "GTIN-13" "GS1 Global Trade Item Number, formerly known as EAN article number (13 digits)",
      --         C.code "04" "UPC" "UPC product number (12 digits)",
      --         C.code "05" "ISMN-10" "International Standard Music Number (M plus nine digits). Pre-2008 – now DEPRECATED in ONIX for Books, except where providing historical information for compatibility with legacy systems. It should only be used in relation to products published before 2008 – when ISMN-13 superseded it – and should never be used as the ONLY identifier (it should always be accompanied by the correct ISMN-13)",
      --         C.code "06" "DOI" "Digital Object Identifier (variable length and character set)",
      --         C.code "13" "LCCN" "Library of Congress Control Number (12 characters, alphanumeric)",
      --         C.code "14" "GTIN-14" "GS1 Global Trade Item Number (14 digits)",
      --         C.code "15" "ISBN-13" "International Standard Book Number, from 2007, unhyphenated (13 digits starting 978 or 9791–9799)",
      --         C.code "17" "Legal deposit number" "The number assigned to a publication as part of a national legal deposit process",
      --         C.code "22" "URN" "Uniform Resource Name: note that in trade applications an ISBN must be sent as a GTIN-13 and, where required, as an ISBN-13 – it should not be sent as a URN",
      --         C.code "23" "OCLC number" "A unique number assigned to a bibliographic item by OCLC",
      --         C.code "24" "Co-publisher’s ISBN-13" "An ISBN-13 assigned by a co-publisher. The ‘main’ ISBN sent with ID type code 03 and/or 15 should always be the ISBN that is used for ordering from the supplier identified in Supply Detail. However, ISBN rules allow a co-published title to carry more than one ISBN. The co-publisher should be identified in an instance of the <Publisher> composite, with the applicable <PublishingRole> code",
      --         C.code "25" "ISMN-13" "International Standard Music Number, from 2008 (13-digit number starting 9790)",
      --         C.code "26" "ISBN-A" "Actionable ISBN, in fact a special DOI incorporating the ISBN-13 within the DOI syntax. Begins ‘10.978.’ or ‘10.979.’ and includes a / character between the registrant element (publisher prefix) and publication element of the ISBN, eg 10.978.000/1234567. Note the ISBN-A should always be accompanied by the ISBN itself, using codes 03 and/or 15",
      --         C.code "27" "JP e-code" "E-publication identifier controlled by JPOIID’s Committee for Research and Management of Electronic Publishing Codes",
      --         C.code "28" "OLCC number" "Unique number assigned by the Chinese Online Library Cataloging Center (see http://olcc.nlc.gov.cn)",
      --         C.code "29" "JP Magazine ID" "Japanese magazine identifier, similar in scope to ISSN but identifying a specific issue of a serial publication. Five digits to identify the periodical, plus a hyphen and two digits to identify the issue",
      --         C.code "30" "UPC12+5" "Used only with comic books and other products which use the UPC extension to identify individual issues or products. Do not use where the UPC12 itself identifies the specific product, irrespective of any 5-digit extension – use code 04 instead",
      --         C.code "31" "BNF Control number" "Numéro de la notice bibliographique BNF",
      --         C.code "35" "ARK" "Archival Resource Key, as a URL (including the address of the ARK resolver provided by eg a national library"
      --       ],
      --     C.codeType
      --       "DiscountCodeType"
      --       "Discount code type, List 100"
      --       [ C.code "01" "BIC discount group code" "UK publisher’s or distributor’s discount group code in a format specified by BIC to ensure uniqueness",
      --         C.code "02" "Proprietary discount code" "A publisher’s or supplier’s own code which identifies a trade discount category, as specified in <DiscountCodeTypeName>. The actual discount for each code is set by trading partner agreement (applies to goods supplied on standard trade discounting terms) ",
      --         C.code "03" "Boeksoort" "Terms code used in the Netherlands book trade",
      --         C.code "04" "German terms code" "Terms code used in German ONIX applications",
      --         C.code "05" "Proprietary commission code" "A publisher’s or supplier’s own code which identifies a commission rate category, as specified in <DiscountCodeTypeName>. The actual commission rate for each code is set by trading partner agreement (applies to goods supplied on agency terms) ",
      --         C.code "06" "BIC commission group code" "UK publisher’s or distributor’s commission group code in format specified by BIC to ensure uniqueness. Format is identical to BIC discount group code, but indicates a commission rather than a discount (applies to goods supplied on agency terms) "
      --       ]
      --   ]
      let txt =
            ( case compiled of
                --  TODO: Handle Either properly
                Left err -> show err
                -- Right t -> unpack $ substitute t (yml :: Schema)
                Right t -> unpack $ substitute t codeTypes
            )
      return $ Just txt

render :: Language -> IO ()
render l = do
  c <- compile Code l
  case c of
    Just c_ -> do
      writeFile "./go/code.go" c_
    Nothing -> throw Unreachable

-- m <- compile Model l
-- d <- compile Decoder l
-- r <- compile Reader l

-- case (m, d, r) of
--   (Just m_, Just d_, Just r_) -> do
--     writeFile "./go/model.go" m_
--     writeFile "./go/decoder.go" d_
--     writeFile "./go/reader.go" r_
--   (Just m_, Nothing, Just r_) -> do
--     writeFile "./go/model.go" m_
--     writeFile "./go/reader.go" r_
--   (_, _, _) -> throw Unreachable
