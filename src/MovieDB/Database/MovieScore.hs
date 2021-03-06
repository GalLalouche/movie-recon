{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.MovieScore(
  init,
  addMovieScores,
  movieScores,
  hasMovieScores,
  allMovieScores,
) where

import           Prelude                           hiding (init)

import           Data.Foldable                     (traverse_)
import qualified Data.Map.Strict                   as Map (assocs)
import qualified Data.Set                          as Set (fromList)
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector (fromList)

import           Control.Arrow                     ((&&&))
import           Control.Monad.Trans.Maybe         (MaybeT(MaybeT))

import           MovieDB.Database                  (DbCall, DbMaybe)
import           MovieDB.Database.Internal.Common  (getKeyFor, getValueByRowId, runInit)
import           MovieDB.Database.Internal.TypesTH ()
import           MovieDB.Database.Movie            (MovieRowId, MovieRowable)
import           OMDB                              (MovieScore(MovieScore), MovieScores(MovieScores, _movie, _scores), Source)

import           Database.Persist.Sql              (entityVal, insert, selectList, (==.))
import           Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import qualified Common.Maps                       as Maps
import           Common.MaybeTs                    (isJust)
import qualified Common.Sets                       as Sets


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieScoreRow sql=movie_score
  movieId         MovieRowId
  source          Source
  score           Int
  UniqueMovieId   movieId source
|]

init :: DbCall ()
init = runInit migrateTables


addMovieScores :: MovieScores -> DbCall ()
addMovieScores (MovieScores m ss) = do
  mid <- getKeyFor m
  let scores = (mid, ) Sets.<$> ss
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
  mid <- getKeyFor m
  result <- map entityVal <$> selectList [MovieScoreRowMovieId ==. mid] []
  if null result then return Nothing else Just <$> toMovieScores result

hasMovieScores :: MovieRowable m => m -> DbCall Bool
hasMovieScores = isJust . movieScores

allMovieScores :: DbCall (Vector MovieScores)
allMovieScores = do
  result <- fmap (pure . entityVal) <$> selectList [] []
  scores <- traverse toMovieScores result
  let movieScoresPairs = Map.assocs $ Maps.semigroupMapBy _movie _scores scores
  return $ Vector.fromList $ map (uncurry MovieScores) movieScoresPairs
