{-# LANGUAGE DuplicateRecordFields, ScopedTypeVariables, QuasiQuotes #-}

module MovieDB.API where

import Entity hiding (Movie)
import Data.Text (Text)
import Common.Operators
import Control.Monad ((>=>))
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Network.HTTP (simpleHTTP, getRequest)
import Data.String.Interpolate

newtype PersonId = PersonId { id :: Text }
newtype MovieId = MovieId { id :: Text }
newtype ApiKey = ApiKey { key :: Text }
data Movie = Movie { id :: MovieId, name :: Text}

type ApiCall a = ReaderT ApiKey IO a

searchPerson :: Text -> ApiCall PersonId
searchPerson t = undefined
--do
--  apiKey <- ask <$$> key
--  let res = liftIO $ simpleHTTP (getRequest [i|https://api.themoviedb.org/3/search/person?api_key=#{key}&language=en-US&page=1&include_adult=false&query=#{t}|]) >>= fmap (take 100) . getResponseBody
--  undefined

movieIds :: PersonId -> ApiCall [MovieId]
movieIds = undefined

movieInfo :: MovieId -> ApiCall Movie
movieInfo = undefined

movies :: PersonId -> ApiCall [Movie]
movies = movieIds >=> traverse movieInfo
