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

compile :: Renderer -> Language -> IO (Maybe String)
compile _ TypeScript = throw Unimplemented
compile r Go = do
  let searchSpace = [".", "src"]
   in case r of
        Code -> do
          let name = "code.mustache"
          compiled <- automaticCompile searchSpace name
          codeTypes <- C.readSchema
          let txt =
                ( case compiled of
                    Left err -> throw $ ParseErr err
                    Right t -> unpack $ substitute t codeTypes
                )
          return $ Just txt
        Model -> do
          let name = "model.mustache"
          compiled <- automaticCompile searchSpace name
          models <- M.readSchema
          let txt =
                ( case compiled of
                    Left err -> throw $ ParseErr err
                    Right t -> unpack $ substitute t models
                )
          return $ Just txt
        Mixed -> do
          let name = "mixed.mustache"
          compiled <- automaticCompile searchSpace name
          models <- Mi.readSchema
          let txt =
                ( case compiled of
                    Left err -> throw $ ParseErr err
                    Right t -> unpack $ substitute t models
                )
          return $ Just txt
        _ -> throw Unimplemented

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
