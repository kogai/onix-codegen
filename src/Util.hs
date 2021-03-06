{-# LANGUAGE OverloadedStrings #-}

module Util
  ( Empty (..),
    throw,
    unwrap,
    unreachable,
    unimplemented,
    GenSchema (..),
    trace',
    typeNameToReferenceName,
    uniq,
    sanitizeName,
    uniqBy,
    unwords',
  )
where

import Control.Exception (Exception, throw)
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Text as T
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

unwords' :: [T.Text] -> T.Text
unwords' = T.pack . unwords . map T.unpack

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

uniqBy :: ([a] -> a -> Bool) -> [a] -> [a]
uniqBy f = foldl (\acc x -> if f acc x then acc else acc ++ [x]) []

typeNameToReferenceName :: T.Text -> T.Text -> T.Text
typeNameToReferenceName name "string" = T.toTitle name
typeNameToReferenceName name "token" = T.toTitle name
typeNameToReferenceName name "anySimpleType" = T.toTitle name
typeNameToReferenceName name x =
  if T.isPrefixOf "List" x
    then T.concat [T.toTitle name, x]
    else sanitizeName x

sanitizeName :: T.Text -> T.Text
sanitizeName x =
  case T.splitOn "." x of
    [] -> x
    [y] -> y
    y : ys -> T.concat $ [T.toTitle y, "Dot"] ++ ys

class (ToMustache a) => GenSchema a where
  readSchema :: Schema -> a
