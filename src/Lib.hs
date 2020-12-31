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

-- import qualified Code as C
import Control.Exception (Exception, throw)
-- import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Yaml (FromJSON (..))
import qualified Data.Yaml as Y
-- import qualified Data.Yaml.Aeson as A
import GHC.Generics
import qualified Schema as S
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

-- type Schema = HashMap Text SchemaNode

-- type Schema = Vector SchemaNode

-- - Value: "01"
--   Description: Proprietary
--   Notes: For example, a publisher’s or wholesaler’s product number. Note that <IDTypeName> is required with proprietary identifiers

compile :: Renderer -> Language -> IO (Maybe String)
compile _ TypeScript = throw Unimplemented
compile r Go = do
  let searchSpace = [".", "src"]
   in case r of
        Code -> do
          let name = "code.mustache"
          compiled <- automaticCompile searchSpace name
          codeTypes <- S.readSchema
          let txt =
                ( case compiled of
                    --  TODO: Handle Either properly
                    Left err -> show err
                    Right t -> unpack $ substitute t codeTypes
                )
          return $ Just txt
        Model -> do
          let name = "model.mustache"
          compiled <- automaticCompile searchSpace name
          codeTypes <- S.readSchema
          let txt =
                ( case compiled of
                    --  TODO: Handle Either properly
                    Left err -> show err
                    Right t -> unpack $ substitute t codeTypes
                )
          return $ Just txt
        _ -> throw Unimplemented

-- case templateName of
--   Nothing -> return Nothing
--   Just "code" -> do
--   Just n -> do
--     let name = n ++ ".mustache"
--     compiled <- automaticCompile searchSpace name
--     -- yml <- Y.decodeFileThrow "./src/schema.yml"
--     codeTypes <- S.readSchema
--     let txt =
--           ( case compiled of
--               --  TODO: Handle Either properly
--               Left err -> show err
--               -- Right t -> unpack $ substitute t (yml :: Schema)
--               Right t -> unpack $ substitute t codeTypes
--           )
--     return $ Just txt

render :: Language -> IO ()
render l = do
  -- c <- compile Code l
  -- case c of
  --   Just c_ -> do
  --     writeFile "./go/code.go" c_
  --   Nothing -> throw Unreachable

  m <- compile Model l
  -- d <- compile Decoder l
  -- r <- compile Reader l
  case m of
    Just c_ -> do
      writeFile "./go/model.go" c_
    Nothing -> throw Unreachable

-- case (m, d, r) of
--   (Just m_, Just d_, Just r_) -> do
--     writeFile "./go/model.go" m_
--     writeFile "./go/decoder.go" d_
--     writeFile "./go/reader.go" r_
--   (Just m_, Nothing, Just r_) -> do
--     writeFile "./go/model.go" m_
--     writeFile "./go/reader.go" r_
--   (_, _, _) -> throw Unreachable
