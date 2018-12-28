{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                                                           #-}

module MovieDB.Database.SeenMovies(
  init,
  clear,
) where

import Prelude                    hiding (init)

import Common.Operators

import MovieDB.Database.Common    (DbCall, path)
import MovieDB.Database.Movies    (MovieRowId, MovieRowable, toMovieRowId)

import Control.Monad              ((>=>))
import Control.Monad.Trans.Reader (ask)

import Database.Persist.Sql       (Entity, Filter, deleteBy, deleteWhere, insert)
import Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
SeenMovies
  movieId         MovieRowId
  UniqueMovieId   movieId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter SeenMovies])

addSeenMovie :: MovieRowable m => m -> DbCall SeenMoviesId
addSeenMovie = toMovieRowId >=> (withMigration . insert . SeenMovies)

removeSeenMovie :: MovieRowable m => m -> DbCall ()
removeSeenMovie = toMovieRowId >=> (withMigration . deleteBy . UniqueMovieId)
