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
import Text.Mustache (automaticCompile, substitute)
import Util

data Language
  = Go
  | TypeScript

data Renderer
  = Model
  | Mixed
  | Code
  | Decoder
  | Reader
  deriving (Show)

templateToGoV2 :: Language -> [FilePath]
templateToGoV2 TypeScript = throw Unimplemented
templateToGoV2 Go = [".", "template", "go", "v2"]

generatedToGeV2 = "generated/go/v2"

compile :: Renderer -> Language -> IO (Maybe String)
compile _ TypeScript = throw Unimplemented
compile Code Go = do
  let name = "code.mustache"
  compiled <- automaticCompile (templateToGoV2 Go) name
  codeTypes <- C.readSchema
  let txt =
        ( case compiled of
            Left err -> throw $ ParseErr err
            Right t -> unpack $ substitute t codeTypes
        )
  return $ Just txt
compile Model Go = do
  let name = "model.mustache"
  compiled <- automaticCompile (templateToGoV2 Go) name
  models <- M.readSchema
  let txt =
        ( case compiled of
            Left err -> throw $ ParseErr err
            Right t -> unpack $ substitute t models
        )
  return $ Just txt
compile Mixed Go = do
  let name = "mixed.mustache"
  compiled <- automaticCompile (templateToGoV2 Go) name
  models <- Mi.readSchema
  let txt =
        ( case compiled of
            Left err -> throw $ ParseErr err
            Right t -> unpack $ substitute t models
        )
  return $ Just txt
compile _ _ = throw Unimplemented

render :: Language -> IO ()
render l = do
  mi <- compile Mixed l
  case mi of
    Just c_ -> do
      writeFile "./go/mixed.go" c_
    Nothing -> throw Unreachable

  c <- compile Code l
  case c of
    Just c_ -> do
      writeFile "./go/code.go" c_
    Nothing -> throw Unreachable

  m <- compile Model l
  case m of
    Just c_ -> do
      writeFile "./go/model.go" c_
    Nothing -> throw Unreachable

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
