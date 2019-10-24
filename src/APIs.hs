{-# LANGUAGE QuasiQuotes #-}

module APIs where

import           Data.String.Interpolate (i)
import           Data.Text               (Text, pack, unpack)

import           Common.JsonUtils        (ObjectParser)
import qualified Common.JsonUtils        as JU (parseUnsafe)
import           Network.HTTP            (getRequest, getResponseBody, simpleHTTP)


readKey :: FilePath -> IO String
readKey fileName = let fullPath = [i|keys/#{fileName}.txt|] in readFile fullPath

newtype Url = Url Text

parseRemoteJson :: Url -> ObjectParser a -> IO a
parseRemoteJson (Url address) parser = let
  request = getRequest $ unpack address
  parse = JU.parseUnsafe parser . pack
  in parse <$> (simpleHTTP request >>= getResponseBody)
