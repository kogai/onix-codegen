-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( render,
    compile,
    Language (..),
  )
where

import Control.Exception (Exception, throw)
import Data.Text (unpack)
import Data.Typeable (Typeable)
-- import Data.Vector (Vector)
-- import Data.Yaml (FromJSON (..))
-- import qualified Data.Yaml as Y
-- import GHC.Generics
import qualified Model as M
import qualified Schema as S
import Text.Mustache (automaticCompile, substitute)
-- import qualified Text.Mustache.Types as MT
import Text.Parsec.Error (ParseError)

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
  | ParseErr ParseError
  deriving (Show, Typeable)

instance Exception Empty

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
                    Left err -> throw $ ParseErr err
                    Right t -> unpack $ substitute t codeTypes
                )
          return $ Just txt
        Model -> do
          let name = "model.mustache"
          compiled <- automaticCompile searchSpace name
          let models =
                M.models
                  [ M.model
                      "header"
                      "Header"
                      M.Tag
                      [ M.model "m174" "FromCompany" M.Tag [],
                        M.model "m182" "SentDate" M.Tag [],
                        M.model "m184" "DefaultLanguageOfText" M.Tag [],
                        M.model "m185" "DefaultPriceTypeCode" M.Tag [],
                        M.model "m186" "DefaultCurrencyCode" M.Tag []
                      ]
                  ]
          let txt =
                ( case compiled of
                    Left err -> throw $ ParseErr err
                    Right t -> unpack $ substitute t models
                )
          return $ Just txt
        _ -> throw Unimplemented

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
