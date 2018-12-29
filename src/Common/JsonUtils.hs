{-# LANGUAGE FlexibleInstances #-}

{-| A bunch of patently unsafe functions, since type-safe/total JSON is the way to madness. |-}
module Common.JsonUtils where

import           Data.Aeson                (Array, Object, Result(..), Value, decode, fromJSON, withArray,
                                            withObject)
import qualified Data.Aeson                as Aeson ((.:))
import           Data.Aeson.Types          (FromJSON, Parser)
import           Data.Vector               (Vector)

import           Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import           Data.Text                 (Text)
import           Data.Text.Lazy            (fromStrict)
import           Data.Text.Lazy.Encoding   (encodeUtf8)

import           Common.Operators
import           Data.Maybe                (fromJust)
import           Data.Traversable          (traverse)


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
get = flip (Aeson..:)

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
fromSuccess (Error e)   = error e
