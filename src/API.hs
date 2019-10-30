{-# LANGUAGE QuasiQuotes #-}

module API where

import           Prelude                       hiding (readFile)

import           Control.Monad.Trans.Maybe     (MaybeT)
import           Data.Text                     (Text, pack, unpack)
import           Data.Text.IO                  (readFile)
import           Text.InterpolatedString.Perl6 (qq)

import           Common.JsonUtils              (ObjectParser)
import qualified Common.JsonUtils              as JU (parseUnsafe)
import           Network.HTTP                  (getRequest, getResponseBody, simpleHTTP)


type ApiCall = IO
type ApiMaybe = MaybeT ApiCall
readKey :: FilePath -> IO Text
readKey fileName = let fullPath = [qq|keys/$fileName.txt|] in readFile fullPath

newtype Url = Url Text

parseRemoteJson :: Url -> ObjectParser a -> IO a
parseRemoteJson (Url address) parser = let
  request = getRequest $ unpack address
  parse = JU.parseUnsafe parser . pack
  in parse <$> (simpleHTTP request >>= getResponseBody)
