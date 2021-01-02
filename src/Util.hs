module Util
  ( Empty (..),
    throw,
    unwrap,
  )
where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Text.Parsec.Error (ParseError)

data Empty
  = Unimplemented
  | Unreachable
  | ParseErr ParseError
  deriving (Show, Typeable)

instance Exception Empty

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = throw Unreachable
