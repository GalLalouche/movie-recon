module Recon where

import Entity

type Reconciler = EntityName -> IO ReconId
