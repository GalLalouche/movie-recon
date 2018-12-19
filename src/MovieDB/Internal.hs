{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module MovieDB.Internal where

import MovieDB.Types

import Entity hiding (Movie)
import Data.Text (Text, pack)
import Control.Monad ((>=>))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import qualified Data.ByteString.Lazy.UTF8 as T
import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import Data.String.Interpolate (i)
import Data.Maybe (fromJust)

import Common.Operators
import Common.Foldables (headUnsafe)

import Common.JsonUtils (asObject, array, object, objects, int, fromSuccess)


newtype ApiKey = ApiKey { key :: Text }
type ApiCall a = ReaderT ApiKey IO a

right :: Either a b -> b
right (Right a) = a

apiCall :: Query -> ApiCall J.Value
apiCall q = do
  apiKey <- ask <$$> key
  let request = [i|http://api.themoviedb.org/3/#{query q}&api_key=#{apiKey}|]
  let res = simpleHTTP (getRequest request) >>= getResponseBody
  liftIO $ res <$$> (T.fromString .> J.eitherDecode .> right)

searchPerson :: Text -> ApiCall PersonId
searchPerson t = undefined

movieIds :: PersonId -> ApiCall [MovieId]
movieIds = undefined

movieInfo :: MovieId -> ApiCall Movie
movieInfo = undefined

movies :: PersonId -> ApiCall [Movie]
movies = undefined
