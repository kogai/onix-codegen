module Lib
  ( someFunc,
    render,
    compile,
    Language (..),
  )
where

import Control.Exception (Exception, throw)
import Data.Text (unpack)
import Data.Typeable
import Text.Mustache

someFunc :: String -> IO ()
someFunc = putStrLn

data Language
  = Go
  | TypeScript

data Empty = Unimplemented
  deriving (Show, Typeable)

instance Exception Empty

compile :: Language -> IO String
compile TypeScript = throw Unimplemented
compile Go = do
  let searchSpace = [".", "src"]
      templateName = "model.mustache"
  compiled <- automaticCompile searchSpace templateName
  return
    -- TODO: Handle Either properly
    ( case compiled of
        Left err -> show err
        Right t -> unpack $ substitute t ()
    )

render :: Language -> IO ()
render l = do
  s <- compile l
  writeFile "./go/onix.gen.go" s
