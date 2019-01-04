module MovieDB.API(
  ApiCall,
  I.ApiKey(..),
  I.ApiQuery,
  runQuery,
  I.MovieCredits(..),
  castAndCrew,
  I.PersonCredits(..),
  personCredits,
) where

import           MovieDB.API.Internal       (ApiCall)
import qualified MovieDB.API.Internal       as I

import           Common.JsonObjectParser    (parseObject)
import           Common.JsonUtils           (decodeUnsafe, fromSuccess)
import           Common.Operators

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)

import qualified Data.List.NonEmpty         as NEL (fromList)
import           Data.Text                  (unpack)

import           MovieDB.Types              (CastAndCrew(..), Movie(..), Participation(..), ParticipationType,
                                             Person(..), toCastAndCrew)

import           Network.HTTP               (getRequest, getResponseBody, simpleHTTP)

runQuery :: I.ApiQuery q r => q -> ApiCall r
runQuery q = do
  request <- ask <$$> I.apiPath q <$$> unpack
  let res = simpleHTTP (getRequest request) >>= getResponseBody
  json <- liftIO $ res <$$> decodeUnsafe
  return $ fromSuccess $ parseObject I.parse json

castAndCrew :: Movie -> ApiCall CastAndCrew
castAndCrew m = do
  ps <- participations m <$> runQuery (I.forMovie m)
  return $ toCastAndCrew $ NEL.fromList ps where
    participations :: Movie -> [(Person, ParticipationType)] -> [Participation]
    participations movie = map aux where aux (p, pt) = Participation p movie pt

personCredits :: Person -> ApiCall [Participation]
personCredits p = do
  ps <- runQuery $ I.forPerson p
  return $ map participations ps where
    participations :: (Movie, ParticipationType) -> Participation
    participations (m, pt) = Participation p m pt
