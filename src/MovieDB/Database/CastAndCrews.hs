module MovieDB.Database.CastAndCrews (
  getCastAndCrew,
  addCastAndCrew,
) where

import           MovieDB.Types                   (CastAndCrew(..), MovieId)

import qualified MovieDB.Database.Acted          as InstancesOnly
import           MovieDB.Database.Common         (DbCall, DbMaybe, getRowId, getValue)
import qualified MovieDB.Database.Directed       as InstancesOnly
import           MovieDB.Database.Movies         (MovieRowId)
import           MovieDB.Database.Participations (addValueEntry, crew)
import qualified MovieDB.Database.Wrote          as InstancesOnly

import           Control.Monad.Trans.Maybe       (MaybeT(..))
import           Data.Functor                    (void)


getCastAndCrew :: MovieId -> DbMaybe CastAndCrew
getCastAndCrew movieId = do
  movieRowId <- getRowId movieId :: DbMaybe MovieRowId
  movie <- getValue movieId
  CastAndCrew <$> return movie <*> getCrew movie <*> getCrew movie <*> getCrew movie where
    getCrew movie = MaybeT $ Just <$> crew movie

-- No row ID to return since CastAndCrew doesn't keep its own database. One *could* return all the row IDS for
-- the Directed, Wrote, and Acted instances, but why bother?
addCastAndCrew :: CastAndCrew -> DbCall ()
addCastAndCrew (CastAndCrew movie ds ws as) = void $
    traverse (addValueEntry movie) ds >>
    traverse (addValueEntry movie) ws >>
    traverse (addValueEntry movie) as
