module Common.JsonObjectParser where

import           Data.Aeson                (Array, Object, Result(..), Value, decode, fromJSON, withArray,
                                            withObject)
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types                as AesonT
import           Data.Aeson.Types          (FromJSON, Parser)
import           Data.Vector               (Vector)

import           Data.ByteString.Lazy.UTF8 (ByteString, fromString)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text, unpack)
import           Data.Text.Lazy            (fromStrict)
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import           Data.Traversable          (traverse)

import           Control.Applicative
import           Control.Monad             (ap, liftM, (>=>), join)

import qualified Common.JsonUtils          as JU
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

get :: FromJSON a => Text -> ObjectParser a
get = ObjectParser . JU.get

int :: Text -> ObjectParser Int
int = ObjectParser . JU.int

str :: Text -> ObjectParser Text
str = ObjectParser . JU.str

strRead :: Read a => Text -> ObjectParser a
strRead = ObjectParser . JU.strRead

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
