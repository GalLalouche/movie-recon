{-# LANGUAGE FlexibleInstances #-}

module Common.JsonUtils(
  I.decodeUnsafe,
  I.asObject,
  I.fromSuccess,
  ObjectParser,
  parseUnsafe,
  parseObject,
  int,
  str,
  strMaybe,
  strRead,
  array,
  object,
  (\\),
  (\>),
  withObjects
) where

import           Data.Aeson                (Array, Object, Result, Value)
import           Data.Aeson.Types          (FromJSON, Parser)
import qualified Data.Aeson.Types          as AesonT

import           Data.String               (IsString(fromString))
import           Data.Text                 (Text, pack)
import           Data.Vector               (Vector)

import           Control.Monad             (ap, liftM, (>=>))

import qualified Common.JsonUtils.Internal as I


newtype ObjectParser a = ObjectParser { parse :: Object -> Parser a }
instance Functor ObjectParser where
  fmap = liftM
instance Applicative ObjectParser where
  pure = return
  (<*>) = ap
instance Monad ObjectParser where
  return = ObjectParser . const . return
  (ObjectParser orig) >>= f = ObjectParser aux where
    aux obj = do
      parser <- parse . f <$> orig obj
      parser obj

parseUnsafe :: ObjectParser a -> Text -> a
parseUnsafe parser = I.fromSuccess . parseObject parser . I.decodeUnsafe

fromValue :: ObjectParser a -> (Value -> Parser a)
fromValue = (I.asObject >=>) . parse

parseObject :: ObjectParser a -> Value -> Result a
parseObject = AesonT.parse . fromValue

int :: Text -> ObjectParser Int
int = ObjectParser . I.int

str :: Text -> ObjectParser Text
str = ObjectParser . I.str

strMaybe :: Text -> ObjectParser (Maybe Text)
strMaybe = ObjectParser . I.strMaybe

strRead :: Read a => Text -> ObjectParser a
strRead = ObjectParser . I.strRead

array :: Text -> ObjectParser Array
array = ObjectParser . I.array

object :: Text -> ObjectParser Object
object = ObjectParser . I.object

objects :: Text -> ObjectParser (Vector Object)
objects = ObjectParser . I.objects

withObjects :: Text -> ObjectParser a -> ObjectParser (Vector a)
withObjects t p = ObjectParser $ \o -> do
  os <- parse (objects t) o :: Parser (Vector Object)
  traverse (parse p) os


combine :: (Text -> b -> Parser a) -> ObjectParser b -> Text -> ObjectParser a
combine combiner (ObjectParser p) fieldName = ObjectParser $ p >=> combiner fieldName

(\\) :: ObjectParser Object -> Text -> ObjectParser Object
(\\) = combine I.object

(\>) :: FromJSON p => ObjectParser Object -> Text -> ObjectParser p
(\>) = combine I.get

-- This allows one to write "foo" \\ "bar" \> "bazz", instead of JU.object "foo" \\ "bar" \> "bazz".
-- Unfortunately, using a "union-class" of Text and (ObjectParser Object) would break when used in combination with
-- OverloadedStrings (which one pretty much has to use for the above to work).
instance IsString (ObjectParser Object) where
   fromString = object . pack
