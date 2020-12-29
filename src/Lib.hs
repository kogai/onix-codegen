module Lib
  ( someFunc,
    render,
    Language (..),
  )
where

import Control.Exception (Exception, throw)
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

render :: Language -> IO ()
render TypeScript = throw Unimplemented
render Go = do
  let searchSpace = ["."]
      templateName = "model.mustache"

  compiled <- automaticCompile searchSpace templateName
  case compiled of
    Left err -> print err
    Right t -> print t
