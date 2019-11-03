{-# LANGUAGE TemplateHaskell #-}

module MovieDB.Database.Internal.TypesTH where

import Database.Persist.TH (derivePersistField)
import MovieDB.Types       (ExternalHost, FilterReason, ParticipationType)
import OMDB                (Source)


-- These have to be in their own file.
-- See https://www.yesodweb.com/book/persistent#persistent_custom_fields
derivePersistField "ParticipationType"
derivePersistField "FilterReason"
derivePersistField "Source"
derivePersistField "ExternalHost"
