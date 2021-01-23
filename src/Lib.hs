{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( render,
    compile,
    Language (..),
    SchemaVersion (..),
  )
where

import qualified Code as C
import Data.Text (unpack)
import qualified Mixed as Mi
import qualified Model as M
import Text.Mustache (Template, automaticCompile, substitute)
import Text.Parsec.Error (ParseError)
import Util
import qualified Xsd as X

data SchemaVersion
  = V2
  | V3

instance Show SchemaVersion where
  show V2 = "v2"
  show V3 = "v3"

data Language
  = Go
  | TypeScript

ext :: Language -> String
ext Go = ".go"
ext TypeScript = ".ts"

data Renderer
  = Model
  | Mixed
  | Code
  | Reader
  deriving (Show)

file :: Renderer -> String
file Model = "model"
file Mixed = "mixed"
file Code = "code"
file Reader = "reader"

template :: Language -> SchemaVersion -> [FilePath]
template TypeScript version = [".", "template/typescript/" ++ show version]
template Go version = [".", "template/go/" ++ show version]

compiledTemplate :: Renderer -> Language -> SchemaVersion -> IO (Either ParseError Template)
compiledTemplate Code l version = automaticCompile (template l version) "code.mustache"
compiledTemplate Model l version = automaticCompile (template l version) "model.mustache"
compiledTemplate Mixed l version = automaticCompile (template l version) "mixed.mustache"
compiledTemplate Reader l version = automaticCompile (template l version) "reader.mustache"

generateTo :: Language -> SchemaVersion -> String
generateTo Go V2 = "generated/go/v2"
generateTo Go V3 = "generated/go/v3"
generateTo TypeScript V2 = "generated/typescript/v2"
generateTo TypeScript V3 = "generated/typescript/v3"

fileName :: Renderer -> Language -> String
fileName r l = file r ++ ext l

compile :: Renderer -> Language -> SchemaVersion -> IO String
compile r l version = do
  compiled <- compiledTemplate r l version
  xsd <- X.getSchema schemaRoot
  return $
    case (compiled, r) of
      (Left err, _) -> throw $ ParseErr err
      (Right t, Code) -> unpack $ substitute t (readSchema xsd :: C.CodeTypes)
      (Right t, Mixed) -> unpack $ substitute t (readSchema xsd :: [Mi.Mixed])
      (Right t, Model) -> unpack $ substitute t (readSchema xsd :: M.Models)
      (Right t, Reader) -> unpack $ substitute t ()
  where
    schemaRoot =
      "./schema"
        ++ ( case version of
               V2 -> "/v2/ONIX_BookProduct_Release2.1_reference.xsd"
               V3 -> "/v3/ONIX_BookProduct_3.0_reference.xsd"
           )

render :: Language -> SchemaVersion -> IO ()
render l version = do
  compile Mixed l version >>= writeFile (generateTo l version ++ "/" ++ fileName Mixed l)
  compile Code l version >>= writeFile (generateTo l version ++ "/" ++ fileName Code l)
  compile Model l version >>= writeFile (generateTo l version ++ "/" ++ fileName Model l)
  compile Reader l version >>= writeFile (generateTo l version ++ "/" ++ fileName Reader l)
