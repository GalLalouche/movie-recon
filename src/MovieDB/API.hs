{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}


module MovieDB.API(
  ApiCall,
  castAndCrew,
  personCredits,
  personName,
  imdbId,
) where

import qualified Data.List.NonEmpty            as NEL (fromList)
import           Data.Text                     (Text)
import           Text.InterpolatedString.Perl6 (qq)

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Maybe     (MaybeT(..))

import qualified MovieDB.API.Internal          as I
import           MovieDB.Types                 (CastAndCrew(..), ImdbId, Movie(..), Participation(..), ParticipationType, Person(..), pattern PersonId, deepId, toCastAndCrew)

import           APIs                          (ApiCall, ApiMaybe, Url)
import qualified APIs

import           Common.JsonUtils              (ObjectParser)
import           Common.Operators


castAndCrew :: Movie -> ApiCall CastAndCrew
castAndCrew m = getParticipations (flip Participation m) [qq|movie/{deepId m}/credits|] I.parseMovieCredits <$$>
    toCastAndCrew . NEL.fromList

personCredits :: Person -> ApiCall [Participation]
personCredits p = getParticipations (Participation p) [qq|person/{deepId p}/movie_credits|] I.parsePersonCredits

personName :: Url -> ApiCall Person
personName url = do
  let pid@(PersonId id) = I.parseId url
  name <- runQuery [qq|person/$id|] I.parsePersonName
  return $ Person {_id = pid, _name = name}

imdbId :: Movie -> ApiMaybe ImdbId
imdbId m = MaybeT $ runQuery [qq|movie/{deepId m}/external_ids|] I.parseImdbId


runQuery :: Text -> ObjectParser r -> ApiCall r
runQuery query parser = do
  key <- APIs.readKey "moviedb"
  let request = APIs.Url [qq|http://api.themoviedb.org/3/$query?api_key=$key&language=en-US|]
  liftIO $ APIs.parseRemoteJson request parser

getParticipations ::
    (b -> ParticipationType -> Participation)
    -> Text
    -> ObjectParser [(b, ParticipationType)]
    -> ApiCall [Participation]
getParticipations toParticipations = runQuery >$$> map (uncurry toParticipations)
