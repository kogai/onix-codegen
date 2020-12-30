{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( render,
    compile,
    Language (..),
  )
where

import Control.Exception (Exception, throw)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import Data.Yaml (FromJSON (..))
import qualified Data.Yaml as Y
import GHC.Generics
import Text.Mustache (ToMustache, automaticCompile, substitute, toMustache)
import Text.Mustache.Types (Object)

data Language
  = Go
  | TypeScript

data Renderer
  = Model
  | Decoder
  | Reader

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

data SchemaBody = SchemaBody
  { key :: String,
    xmlRefrenceName :: String,
    description :: String,
    kind :: SchemaKind,
    -- TODO: Should be injectable by Language respectively.
    model :: String
  }
  deriving (Generic, Show)

instance FromJSON SchemaBody

data SchemaNode
  = Leaf SchemaBody
  | Node
      { children :: [SchemaNode],
        body :: SchemaBody
      }
  deriving (Generic, Show)

instance FromJSON SchemaNode

-- instance ToMustache Schema where
--   toMustache = Object . fmap toMustache

type Schema = HashMap Text SchemaNode

-- instance FromJSON Schema

-- ToMustache = id

--   toMustache s = throw Unimplemented

-- M.object
--   [("headers", (M.~= "headers" headers))]

-- [M.~= "headers" headers]

compile :: Renderer -> Language -> IO (Maybe String)
compile _ TypeScript = throw Unimplemented
compile r Go = do
  let searchSpace = [".", "src"]
      templateName = case r of
        Model -> Just "model"
        Decoder -> Nothing
        Reader -> Just "reader"

  case templateName of
    Nothing -> return Nothing
    Just n -> do
      let name = n ++ ".mustache"
      compiled <- automaticCompile searchSpace name
      -- TODO: Use Either
      yml <- Y.decodeFileThrow "./src/schema.yml"
      print yml
      let txt =
            ( case compiled of
                --  TODO: Handle Either properly
                Left err -> show err
                Right t -> unpack $ substitute t (yml :: Schema)
            )
      return $ Just txt

render :: Language -> IO ()
render l = do
  m <- compile Model l
  d <- compile Decoder l
  r <- compile Reader l

  case (m, d, r) of
    (Just m_, Just d_, Just r_) -> do
      writeFile "./go/model.go" m_
      writeFile "./go/decoder.go" d_
      writeFile "./go/reader.go" r_
    (Just m_, Nothing, Just r_) -> do
      writeFile "./go/model.go" m_
      writeFile "./go/reader.go" r_
    (_, _, _) -> throw Unreachable
