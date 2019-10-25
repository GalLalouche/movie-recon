{-# LANGUAGE QuasiQuotes #-}

module APIs where

import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.String.Interpolate   (i)
import           Data.Text                 (Text, pack, unpack)

import           Common.JsonUtils          (ObjectParser)
import qualified Common.JsonUtils          as JU (parseUnsafe)
import           Network.HTTP              (getRequest, getResponseBody, simpleHTTP)


type ApiCall = IO
type ApiMaybe = MaybeT ApiCall
readKey :: FilePath -> IO String
readKey fileName = let fullPath = [i|keys/#{fileName}.txt|] in readFile fullPath

newtype Url = Url String

parseRemoteJson :: Url -> ObjectParser a -> IO a
parseRemoteJson (Url address) parser = let
  request = getRequest address
  parse = JU.parseUnsafe parser . pack
  in parse <$> (simpleHTTP request >>= getResponseBody)
