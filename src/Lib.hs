module Lib
  ( render,
    compile,
    Language (..),
  )
where

import Control.Exception (Exception, throw)
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Text.Mustache (automaticCompile, substitute)

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

compile :: Renderer -> Language -> IO (Maybe String)
compile _ TypeScript = throw Unimplemented
compile r Go = do
  let searchSpace = [".", "src"]
      templateName = case r of
        Model -> Just "model"
        Decoder -> Nothing
        Reader -> Just "reader"
  -- )
  case templateName of
    Nothing -> return Nothing
    Just n -> do
      let name = n ++ ".mustache"
      compiled <- automaticCompile searchSpace name
      let txt =
            ( case compiled of
                --  TODO: Handle Either properly
                Left err -> show err
                Right t -> unpack $ substitute t ()
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
