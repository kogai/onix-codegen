module Util
  ( Empty (..),
    throw,
    unwrap,
    GenSchema (..),
  )
where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Text.Mustache (ToMustache)
import Text.Parsec.Error (ParseError)
import Xsd (Schema)

data Empty
  = Unimplemented
  | Unreachable
  | ParseErr ParseError
  deriving (Show, Typeable)

instance Exception Empty

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = throw Unreachable

class (ToMustache a) => GenSchema a where
  readSchema :: Schema -> a
