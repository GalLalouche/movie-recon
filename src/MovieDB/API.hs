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

import           Data.Foldable                 (toList)
import qualified Data.List.NonEmpty            as NEL (fromList)
import           Data.Text                     (Text)
import           Data.Vector                   (Vector)
import           Text.InterpolatedString.Perl6 (qq)

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Maybe     (MaybeT(MaybeT))

import qualified MovieDB.API.Internal          as I
import           MovieDB.Types                 (CastAndCrew(CastAndCrew), ImdbId, Movie(Movie), Participation(Participation), ParticipationType, Person(Person), pattern PersonId, deepId, toCastAndCrew)

import           API                           (ApiCall, ApiMaybe, Url(Url), parseRemoteJson, readKey)

import           Common.JsonUtils              (ObjectParser)
import           Common.Operators              ((<$$>), (>$$>))


castAndCrew :: Movie -> ApiCall CastAndCrew
castAndCrew m = getParticipations (flip Participation m) [qq|movie/{deepId m}/credits|] I.parseMovieCredits <$$>
    toCastAndCrew . NEL.fromList . toList

personCredits :: Person -> ApiCall (Vector Participation)
personCredits p = getParticipations (Participation p) [qq|person/{deepId p}/movie_credits|] I.parsePersonCredits

personName :: Url -> ApiCall Person
personName url = do
  let pid@(PersonId id) = I.parseId url
  name <- runQuery [qq|person/$id|] I.parsePersonName
  return $ Person pid name

imdbId :: Movie -> ApiMaybe ImdbId
imdbId m = MaybeT $ runQuery [qq|movie/{deepId m}/external_ids|] I.parseImdbId


runQuery :: Text -> ObjectParser r -> ApiCall r
runQuery query parser = do
  key <- readKey "moviedb"
  let request = Url [qq|http://api.themoviedb.org/3/$query?api_key=$key&language=en-US|]
  parseRemoteJson request parser

getParticipations ::
    (b -> ParticipationType -> Participation)
    -> Text
    -> ObjectParser (Vector (b, ParticipationType))
    -> ApiCall (Vector Participation)
getParticipations toParticipations = runQuery >$$> fmap (uncurry toParticipations)
