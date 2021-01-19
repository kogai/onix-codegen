{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( render,
    compile,
    Language (..),
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

data Language
  = Go
  | TypeScript

data Renderer
  = Model
  | Mixed
  | Code
  | Reader
  deriving (Show)

templateToGoV2 :: Language -> [FilePath]
templateToGoV2 TypeScript = throw Unimplemented
templateToGoV2 Go = [".", "template/go/v2"]

compiledTemplate :: Renderer -> Language -> IO (Either ParseError Template)
compiledTemplate Code Go = automaticCompile (templateToGoV2 Go) "code.mustache"
compiledTemplate Model Go = automaticCompile (templateToGoV2 Go) "model.mustache"
compiledTemplate Mixed Go = automaticCompile (templateToGoV2 Go) "mixed.mustache"
compiledTemplate Reader Go = automaticCompile (templateToGoV2 Go) "reader.mustache"
compiledTemplate _ TypeScript = throw Unimplemented

generateTo :: Language -> String
generateTo Go = "generated/go/v2"
generateTo TypeScript = "generated/typescript/v2"

compile :: Renderer -> Language -> IO String
compile _ TypeScript = throw Unimplemented
compile r Go = do
  compiled <- compiledTemplate r Go
  xsd <- X.getSchema "./schema/v2/ONIX_BookProduct_Release2.1_reference.xsd"
  return $
    case (compiled, r) of
      (Left err, _) -> throw $ ParseErr err
      (Right t, Code) -> unpack $ substitute t (readSchema xsd :: C.CodeTypes)
      (Right t, Mixed) -> unpack $ substitute t (readSchema xsd :: [Mi.Mixed])
      (Right t, Model) -> unpack $ substitute t (readSchema xsd :: M.Models)
      (Right t, Reader) -> unpack $ substitute t ()

render :: Language -> IO ()
render l = do
  compile Mixed l >>= writeFile (generateTo l ++ "/mixed.go")
  compile Code l >>= writeFile (generateTo l ++ "/code.go")
  compile Model l >>= writeFile (generateTo l ++ "/model.go")
  compile Reader l >>= writeFile (generateTo l ++ "/reader.go")
