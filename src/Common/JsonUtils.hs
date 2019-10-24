module Common.JsonUtils(
  I.decodeUnsafe,
  I.asObject,
  I.fromSuccess,
  ObjectParser,
  parseObject,
  int,
  str,
  strMaybe,
  strRead,
  array,
  object,
  (\\),
  withObjects
) where

import           Data.Aeson                (Array, Object, Result, Value)
import           Data.Aeson.Types          (Parser)
import qualified Data.Aeson.Types          as AesonT

import           Data.Text                 (Text)
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

-- A combinator for Object parsers
-- >  object "foo" \\ object "bar" \\ object "bazz" \\ int "quxx"
(\\) :: ObjectParser Object -> ObjectParser a -> ObjectParser a
(ObjectParser orig) \\ (ObjectParser new) = ObjectParser $ orig >=> new

objects :: Text -> ObjectParser (Vector Object)
objects = ObjectParser . I.objects

withObjects :: Text -> ObjectParser a -> ObjectParser (Vector a)
withObjects t p = ObjectParser $ \o -> do
  os <- parse (objects t) o :: Parser (Vector Object)
  traverse (parse p) os
