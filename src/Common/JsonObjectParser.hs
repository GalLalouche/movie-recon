module Common.JsonObjectParser where

import           Data.Aeson       (Array, Object, Result(..), Value)
import qualified Data.Aeson       as Aeson
import           Data.Aeson.Types (FromJSON, Parser)
import qualified Data.Aeson.Types as AesonT

import           Data.Text        (Text)
import           Data.Traversable (traverse)
import           Data.Vector      (Vector)

import           Control.Monad    (ap, join, liftM, (>=>))

import qualified Common.JsonUtils as JU
import           Common.Operators

newtype ObjectParser a = ObjectParser { parse :: Object -> Parser a }
instance Functor ObjectParser where
  fmap = liftM
instance Applicative ObjectParser where
  pure = return
  (<*>) = ap
instance Monad ObjectParser where
  return = ObjectParser . const . return
  (ObjectParser orig) >>= f = ObjectParser aux where
    aux o = do
      a <- orig o
      (parse $ f a) o

fromValue :: ObjectParser a -> (Value -> Parser a)
fromValue = (JU.asObject >=>) . parse

parseObject :: ObjectParser a -> Value -> Result a
parseObject = AesonT.parse . fromValue

getMaybe :: FromJSON a => Text -> ObjectParser (Maybe a)
getMaybe = ObjectParser . JU.getMaybe

get :: FromJSON a => Text -> ObjectParser a
get = ObjectParser . JU.get

int :: Text -> ObjectParser Int
int = ObjectParser . JU.int

str :: Text -> ObjectParser Text
str = ObjectParser . JU.str

strMaybe :: Text -> ObjectParser (Maybe Text)
strMaybe = ObjectParser . JU.strMaybe

strRead :: Read a => Text -> ObjectParser a
strRead = ObjectParser . JU.strRead

strReadMaybe :: Read a => Text -> ObjectParser (Maybe a)
strReadMaybe = ObjectParser . JU.strReadMaybe

array :: Text -> ObjectParser Array
array = ObjectParser . JU.array

object :: Text -> ObjectParser Object
object = ObjectParser . JU.object

objects :: Text -> ObjectParser (Vector Object)
objects = ObjectParser . JU.objects

flatten :: ObjectParser (Parser a) -> ObjectParser a
flatten (ObjectParser p) = ObjectParser $ join . p

withObjects :: Text -> ObjectParser a -> ObjectParser (Vector a)
withObjects t p = ObjectParser $ \o -> do
  os <- parse (objects t) o :: Parser (Vector Object)
  traverse (parse p) os
