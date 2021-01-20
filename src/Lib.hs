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

data Renderer
  = Model
  | Mixed
  | Code
  | Reader
  deriving (Show)

templateToGoV2 :: Language -> SchemaVersion -> [FilePath]
templateToGoV2 TypeScript _version = throw Unimplemented
templateToGoV2 Go version = [".", "template/go/" ++ show version]

compiledTemplate :: Renderer -> Language -> SchemaVersion -> IO (Either ParseError Template)
compiledTemplate Code Go version = automaticCompile (templateToGoV2 Go version) "code.mustache"
compiledTemplate Model Go version = automaticCompile (templateToGoV2 Go version) "model.mustache"
compiledTemplate Mixed Go version = automaticCompile (templateToGoV2 Go version) "mixed.mustache"
compiledTemplate Reader Go version = automaticCompile (templateToGoV2 Go version) "reader.mustache"
compiledTemplate _ TypeScript _version = throw Unimplemented

generateTo :: Language -> SchemaVersion -> String
generateTo Go V2 = "generated/go/v2"
generateTo Go V3 = "generated/go/v3"
generateTo TypeScript V2 = "generated/typescript/v2"
generateTo TypeScript V3 = "generated/typescript/v3"

compile :: Renderer -> Language -> SchemaVersion -> IO String
compile r Go version = do
  compiled <- compiledTemplate r Go version
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
compile _ TypeScript _version = throw Unimplemented

render :: Language -> SchemaVersion -> IO ()
render l version = do
  compile Mixed l version >>= writeFile (generateTo l version ++ "/mixed.go")
  compile Code l version >>= writeFile (generateTo l version ++ "/code.go")
  compile Model l version >>= writeFile (generateTo l version ++ "/model.go")
  compile Reader l version >>= writeFile (generateTo l version ++ "/reader.go")
