module Recon where

import Entity (EntityName, ReconId)

type Reconciler = EntityName -> IO ReconId
