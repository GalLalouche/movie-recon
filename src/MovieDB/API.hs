{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables                                                        #-}

module MovieDB.API where

import           Common.JsonUtils           (fromSuccess)
import           Common.Operators
import           Common.Unsafe              (right)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)

import           Data.ByteString.Lazy.UTF8  (fromString)
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text, pack)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as AesonT

import qualified MovieDB.Parsers            as P
import           MovieDB.Types              (MovieId(..), ParticipationType, Person)

import           Network.HTTP               (getRequest, getResponseBody, simpleHTTP)

newtype ApiKey = ApiKey { key :: Text }

type ApiCall a = ReaderT ApiKey IO a

newtype MovieCredits = MovieCredits { movieId :: MovieId }

class ApiQuery q r | q -> r, r -> q where
  buildQuery :: q -> Text
  parse :: AesonT.Value -> AesonT.Parser r

instance ApiQuery MovieCredits [(Person, ParticipationType)] where
  buildQuery (MovieCredits mid) = pack [i|movie/#{mid}/credits|]
  parse = P.parseMovieCredits

runQuery :: ApiQuery q r => q -> ApiCall r
runQuery q = do
  apiKey <- ask <$$> key
  let query = buildQuery q
  let request = [i|https://api.themoviedb.org/3/#{query}?api_key=#{apiKey}&language=en-US|]
  let res = simpleHTTP (getRequest request) >>= getResponseBody
  json <- liftIO $ res <$$> (fromString .> Aeson.eitherDecode .> right) :: ApiCall Aeson.Value
  return $ fromSuccess $ AesonT.parse parse json
