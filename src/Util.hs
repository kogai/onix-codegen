module Util
  ( Empty (..),
    throw,
    unwrap,
    unreachable,
    unimplemented,
    GenSchema (..),
    trace',
    uniq,
  )
where

import Control.Exception (Exception, throw)
import Data.List (intercalate)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import Text.Mustache (ToMustache)
import Text.Parsec.Error (ParseError)
import Xsd (Schema)

data Empty
  = Unimplemented
  | Unreachable
  | UnimplementedWithReason [String]
  | UnreachableWithReason String
  | ParseErr ParseError
  deriving (Show, Typeable)

instance Exception Empty

unreachable :: [String] -> a
unreachable xs = throw err
  where
    reason = intercalate "\n" xs
    err = UnreachableWithReason reason

unimplemented :: [String] -> a
unimplemented xs = throw $ UnimplementedWithReason xs

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = throw Unreachable

trace' :: [String] -> a -> a
trace' xs = trace $ unwords ("\n====\n" : xs)

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

class (ToMustache a) => GenSchema a where
  readSchema :: Schema -> a
