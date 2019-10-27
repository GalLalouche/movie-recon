{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.MovieScores(
  init,
  clear,
  addMovieScores,
  movieScores,
  hasMovieScores,
  allMovieScores,
) where

import           Prelude                           hiding (init)

import           Data.Foldable                     (toList, traverse_)
import qualified Data.Map                          as Map (assocs)
import qualified Data.Set                          as Set (fromList)

import           Control.Arrow                     ((&&&))
import           Control.Monad.Trans.Maybe         (MaybeT(..))
import           Data.Functor                      (void)

import           MovieDB.Database                  (DbCall, DbMaybe)
import           MovieDB.Database.Internal.Common  (getValueByRowId)
import           MovieDB.Database.Internal.TypesTH ()
import           MovieDB.Database.Movies           (MovieRowId, MovieRowable, toMovieRowId)
import           OMDB                              (MovieScore(..), MovieScores(..), Source)

import           Database.Persist.Sql              (Filter, deleteWhere, entityVal, insert, runMigrationSilent, selectList, (==.))
import           Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import qualified Common.Maps                       as Maps
import           Common.MaybeTs                    (isJust)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieScoreRow sql=movie_score
  movieId         MovieRowId
  source          Source
  score           Int
  UniqueMovieId   movieId source
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter MovieScoreRow]

clear :: DbCall()
clear = deleteWhere passFilter


addMovieScores :: MovieScores -> DbCall ()
addMovieScores (MovieScores m ss) = do
  mid <- toMovieRowId m
  let scores = (mid, ) <$> toList ss
  traverse_ (uncurry addMovieScore) scores where
    addMovieScore :: MovieRowId -> MovieScore -> DbCall MovieScoreRowId
    addMovieScore mid (MovieScore source score) = insert $ MovieScoreRow mid source score

toMovieScores :: [MovieScoreRow] -> DbCall MovieScores
toMovieScores result = do
  movie <- getValueByRowId $ movieScoreRowMovieId $ head result
  return $ MovieScores movie (Set.fromList $ map toScore result) where
    toScore = uncurry MovieScore . (movieScoreRowSource &&& movieScoreRowScore)

-- Returns nothing if there are no scores for the movie
movieScores :: MovieRowable m => m -> DbMaybe MovieScores
movieScores m = MaybeT $ do
  mid <- toMovieRowId m
  result <- map entityVal <$> selectList [MovieScoreRowMovieId ==. mid] []
  if null result then return Nothing else Just <$> toMovieScores result

hasMovieScores :: MovieRowable m => m -> DbCall Bool
hasMovieScores = isJust . movieScores

allMovieScores :: DbCall [MovieScores]
allMovieScores = do
  result <- fmap (pure . entityVal) <$> selectList passFilter []
  scores <- traverse toMovieScores result
  let movieScoresPairs = Map.assocs $ Maps.semigroupMapBy _movie _scores scores
  return $ map (uncurry MovieScores) movieScoresPairs
