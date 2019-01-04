{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell                                       #-}

module MovieDB.API.Internal where

import           Prelude                    hiding (id)

import           Common.JsonObjectParser    (ObjectParser)

import           Control.Monad.Trans.Reader (ReaderT)

import           Control.Lens               (classUnderscoreNoPrefixFields, makeLensesWith, view, (^.))

import           Data.String.Interpolate    (i)
import           Data.Text                  (Text, pack)

import qualified MovieDB.Parsers            as P
import           MovieDB.Types              (HasDeepId, Movie(..), MovieId, ParticipationType, Person(..),
                                             PersonId, deepId)

newtype ApiKey = ApiKey { key :: Text }

type ApiCall a = ReaderT ApiKey IO a

class ApiQuery q r | q -> r, r -> q where
  buildQuery :: q -> Text
  parse :: ObjectParser r

newtype MovieCredits = MovieCredits { _id :: MovieId }
makeLensesWith classUnderscoreNoPrefixFields ''Movie
makeLensesWith classUnderscoreNoPrefixFields ''MovieCredits
makeLensesWith classUnderscoreNoPrefixFields ''MovieId
instance HasDeepId MovieCredits where deepId = view $ id . id
forMovie :: Movie -> MovieCredits
forMovie = MovieCredits . view id

instance ApiQuery MovieCredits [(Person, ParticipationType)] where
  buildQuery m = pack [i|movie/#{deepId m}/credits|]
  parse = P.parseMovieCredits

newtype PersonCredits = PersonCredits { _id :: PersonId }
makeLensesWith classUnderscoreNoPrefixFields ''Person
makeLensesWith classUnderscoreNoPrefixFields ''PersonCredits
makeLensesWith classUnderscoreNoPrefixFields ''PersonId
instance HasDeepId PersonCredits where deepId = view $ id . id
forPerson :: Person -> PersonCredits
forPerson = PersonCredits . view id

instance ApiQuery PersonCredits [(Movie, ParticipationType)] where
  buildQuery p = pack [i|person/#{deepId p}/movie_credits|]
  parse = P.parsePersonCredits

apiPath :: ApiQuery q r => q -> ApiKey -> Text
apiPath q (ApiKey apiKey) =
  pack [i|http://api.themoviedb.org/3/#{buildQuery q}?api_key=#{apiKey}&language=en-US|]
