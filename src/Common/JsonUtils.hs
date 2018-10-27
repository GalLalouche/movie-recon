{-# LANGUAGE FlexibleInstances #-}

{-| A bunch of patently unsafe functions, since type-safe JSON is the way madness. |-}
module Common.JsonUtils where

import Data.Aeson (Value, Array, Object, (.:), withObject, withArray, decode, fromJSON, Result(..))
import Data.Vector (Vector)
import Data.Aeson.Types (Parser, FromJSON)

import Data.Text (Text)
import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)

import Common.Operators
import Data.Traversable (traverse)
import Data.Maybe (fromJust)
import Control.Monad ((>=>), (<=<))


-- TODO move to common
class ByteStringable a where
  toByteString :: a -> ByteString

instance ByteStringable ByteString where
  toByteString = id

instance ByteStringable String where
  toByteString = fromString

instance ByteStringable Text where
  toByteString = encodeUtf8 . fromStrict

decodeUnsafe :: ByteStringable a => a -> Value
decodeUnsafe = toByteString .> decode .> fromJust

asObject :: Value -> Parser Object
asObject = withObject "object" return

get :: FromJSON a => Text -> Object -> Parser a
get = flip (.:)


int :: Text -> Object -> Parser Int
int = get

str :: Text -> Object -> Parser Text
str = get

array :: Text -> Object -> Parser Array
array = get >==> withArray "array" return

object :: Text -> Object -> Parser Object
object = get >==> withObject "object" return

objects :: Text -> Object -> Parser (Vector Object)
objects = array >==> traverse asObject

fromSuccess :: Result a -> a
fromSuccess (Success a) = a
fromSuccess (Error e) = error e
