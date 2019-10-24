{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module MovieDB.API(
  ApiCall,
  castAndCrew,
  personCredits,
  personName,
  imdbId,
) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Maybe  (MaybeT(..))
import           Control.Monad.Trans.Reader (ReaderT, ask)

import qualified Data.List.NonEmpty         as NEL (fromList)
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text, pack)

import qualified MovieDB.Parsers            as P
import           MovieDB.Types              (CastAndCrew(..), ImdbId(..), Movie(..), Participation(..), ParticipationType, Person(..), PersonId(..), deepId, toCastAndCrew)

import           APIs                       (ApiCall, ApiMaybe)
import qualified APIs

import           Common.JsonUtils           (ObjectParser, parseObject)
import qualified Common.JsonUtils           as JU (decodeUnsafe, fromSuccess)
import           Common.Operators


castAndCrew :: Movie -> ApiCall CastAndCrew
castAndCrew m = getParticipations (flip Participation m) [i|movie/#{deepId m}/credits|] P.parseMovieCredits <$$>
    toCastAndCrew . NEL.fromList

personCredits :: Person -> ApiCall [Participation]
personCredits p = getParticipations (Participation p) [i|person/#{deepId p}/movie_credits|] P.parsePersonCredits

personName :: PersonId -> ApiCall Text
personName (PersonId pid) = runQuery [i|person/#{pid}|] P.parsePersonName


runQuery :: String -> ObjectParser r -> ApiCall r
runQuery query parser = do
  key <- APIs.readKey "moviedb"
  let request = APIs.Url [i|http://api.themoviedb.org/3/#{query}?api_key=#{key}&language=en-US|]
  liftIO $ APIs.parseRemoteJson request parser

getParticipations ::
    (b -> ParticipationType -> Participation)
    -> String
    -> ObjectParser [(b, ParticipationType)]
    -> ApiCall [Participation]
getParticipations toParticipations = runQuery >$$> map (uncurry toParticipations)

imdbId :: Movie -> ApiMaybe ImdbId
imdbId m = MaybeT $ runQuery [i|movie/#{deepId m}/external_ids|] P.parseImdbId
