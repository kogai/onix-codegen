module Lib
  ( someFunc,
    render,
    Language (..),
  )
where

import Control.Exception
import Data.Typeable

someFunc :: String -> IO ()
someFunc = putStrLn

data Language
  = Go
  | TypeScript

data Empty = Empty
  deriving (Show, Typeable)

instance Exception Empty

render :: Language -> String
render Go = "abc"
render TypeScript = throw Empty

-- type IngramContentOnix struct {
-- 	XMLName  xml.Name  `xml:"ONIXmessage"`
-- 	Header   Header    `xml:"header"`
-- 	Products []Product `xml:"product"`
-- }
