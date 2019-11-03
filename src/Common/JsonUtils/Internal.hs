{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-| A bunch of patently unsafe functions, since type-safe/total JSON is the way to madness. |-}
module Common.JsonUtils.Internal(
  ByteStringable(..),
  decodeUnsafe,
  asObject,
  getMaybe,
  get,
  int,
  strMaybe,
  str,
  strRead,
  strReadMaybe,
  array,
  object,
  objects,
  fromSuccess,
) where

import           Data.Aeson                    (Array, Object, Result(Error, Success), Value, decode, withArray, withObject)
import           Data.Aeson.Types              (FromJSON, Parser, parseFieldMaybe)
import           Data.Vector                   (Vector)

import           Data.ByteString.Lazy.UTF8     (ByteString, fromString)
import           Data.Text                     (Text, unpack)
import           Data.Text.Lazy                (fromStrict)
import           Data.Text.Lazy.Encoding       (encodeUtf8)
import           Text.InterpolatedString.Perl6 (qq)
import           Text.Read                     (readMaybe)

import qualified Common.Maybes                 as Maybes
import           Common.Operators


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
decodeUnsafe a = Maybes.orError [qq|Could not decode <{toByteString a}>|] (toByteString a |> decode)

asObject :: Value -> Parser Object
asObject = withObject "object" return

orError :: Text -> (Text -> Object -> Parser (Maybe a)) -> Text -> Object -> Parser a
orError action f = \t o -> f t o <$$> Maybes.orError (msg t o) where
  msg field o = [qq|Could not apply <$action> on text <$field> in object <$o>|]

getMaybe :: FromJSON a => Text -> Object -> Parser (Maybe a)
getMaybe = flip parseFieldMaybe

get :: FromJSON a => Text -> Object -> Parser a
get = orError "get" getMaybe

int :: Text -> Object -> Parser Int
int = get

str :: Text -> Object -> Parser Text
str = get

strMaybe :: Text -> Object -> Parser (Maybe Text)
strMaybe = getMaybe

strReadMaybe :: Read a => Text -> Object -> Parser (Maybe a)
strReadMaybe = str >$$> readMaybe . unpack

strRead :: Read a => Text -> Object -> Parser a
strRead = orError "strRead" strReadMaybe

array :: Text -> Object -> Parser Array
array = get >==> withArray "array" return

object :: Text -> Object -> Parser Object
object = get >==> asObject

objects :: Text -> Object -> Parser (Vector Object)
objects = array >==> traverse asObject

fromSuccess :: Result a -> a
fromSuccess (Success a) = a
fromSuccess (Error e)   = error e
