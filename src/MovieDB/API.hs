module MovieDB.API(
  I.ApiKey(..),
  I.ApiQuery,
  runQuery,
  I.MovieCredits(..),
  castAndCrew,
  I.PersonCredits(..),
  personCredits,
) where

import qualified MovieDB.API.Internal       as I

import           Common.JsonObjectParser    (ObjectParser, parseObject)
import           Common.JsonUtils           (decodeUnsafe, fromSuccess)
import           Common.Operators

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)

import           Control.Lens               (classUnderscoreNoPrefixFields, makeLensesWith, view, (^.))

import qualified Data.List.NonEmpty         as NEL (fromList)
import           Data.String.Interpolate    (i)
import           Data.Text                  (Text, pack, unpack)

import           Data.Aeson                 (Value)

import qualified MovieDB.Parsers            as P
import           MovieDB.Types              (CastAndCrew(..), HasDeepId, Movie(..), MovieId,
                                             Participation(..), ParticipationType, Person(..), PersonId,
                                             deepId, toCastAndCrew)

import           Network.HTTP               (getRequest, getResponseBody, simpleHTTP)

runQuery :: I.ApiQuery q r => q -> I.ApiCall r
runQuery q = do
  request <- ask <$$> I.apiPath q <$$> unpack
  let res = simpleHTTP (getRequest request) >>= getResponseBody
  json <- liftIO $ res <$$> decodeUnsafe :: I.ApiCall Value
  return $ fromSuccess $ parseObject I.parse json

castAndCrew :: Movie -> I.ApiCall CastAndCrew
castAndCrew m = do
  ps <- participations m <$> runQuery (I.forMovie m)
  return $ toCastAndCrew $ NEL.fromList ps where
    participations :: Movie -> [(Person, ParticipationType)] -> [Participation]
    participations movie = map aux where aux (p, pt) = Participation p movie pt

personCredits :: Person -> I.ApiCall [Participation]
personCredits p = do
  ps <- runQuery $ I.forPerson p
  return $ map participations ps where
    participations :: (Movie, ParticipationType) -> Participation
    participations (m, pt) = Participation p m pt
