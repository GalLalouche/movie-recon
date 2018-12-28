{-# LANGUAGE TemplateHaskell #-}

module MovieDB.Database.ParticipationTH where

import Database.Persist.TH (derivePersistField)
import MovieDB.Types       (ParticipationType)

-- This has to be in its own file. See https://www.yesodweb.com/book/persistent#persistent_custom_fields
derivePersistField "ParticipationType"
