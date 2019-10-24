{-# LANGUAGE QuasiQuotes #-}

module MovieDB.API(
  ApiKey(..),
  ApiCall,
  readKey,

  castAndCrew,
  personCredits,
  personName,
) where

import           Common.JsonUtils           (ObjectParser, parseObject)
import qualified Common.JsonUtils           as JU (decodeUnsafe, fromSuccess)
import           Common.Operators

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)

import qualified Data.List.NonEmpty         as NEL (fromList)
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text, pack)

import qualified MovieDB.Parsers            as P
import           MovieDB.Types              (CastAndCrew(..), Movie(..), Participation(..), ParticipationType, Person(..), PersonId(..), deepId, toCastAndCrew)

import           Network.HTTP               (getRequest, getResponseBody, simpleHTTP)


newtype ApiKey = ApiKey { key :: Text }
type ApiCall a = ReaderT ApiKey IO a

readKey :: IO ApiKey
readKey = ApiKey . pack <$> readFile "keys/moviedb.txt"

castAndCrew :: Movie -> ApiCall CastAndCrew
castAndCrew m = getParticipations (flip Participation m) [i|movie/#{deepId m}/credits|] P.parseMovieCredits <$$>
    toCastAndCrew . NEL.fromList

personCredits :: Person -> ApiCall [Participation]
personCredits p = getParticipations (Participation p) [i|person/#{deepId p}/movie_credits|] P.parsePersonCredits

personName :: PersonId -> ApiCall Text
personName (PersonId pid) = runQuery [i|person/#{pid}|] P.parsePersonName


runQuery :: String -> ObjectParser r -> ApiCall r
runQuery query parser = do
  (ApiKey key) <- ask
  let request = [i|http://api.themoviedb.org/3/#{query}?api_key=#{key}&language=en-US|]
  let responseBody = simpleHTTP (getRequest request) >>= getResponseBody
  json <- liftIO $ JU.decodeUnsafe <$> responseBody
  return $ JU.fromSuccess $ parseObject parser json

getParticipations ::
    (b -> ParticipationType -> Participation)
    -> String
    -> ObjectParser [(b, ParticipationType)]
    -> ApiCall [Participation]
getParticipations toParticipations = runQuery >$$> map (uncurry toParticipations)
