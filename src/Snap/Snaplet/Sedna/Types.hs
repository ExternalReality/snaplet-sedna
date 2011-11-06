module Snap.Snaplet.Sedna.Types where

import            Prelude hiding (catch)
import            Control.Monad.IO.Control
import            Control.Monad.State
import            Database.SednaTypes
import            Database.SednaBindings
import            Data.Pool

class ConnSrc s where
  withConn   :: (MonadControlIO m) => s SednaConnection -> (SednaConnection -> m b) -> m b
  closeConn  :: (MonadControlIO m) => s SednaConnection ->  SednaConnection -> m ()

instance ConnSrc Pool where
  withConn       = withResource
  closeConn _ _  = return ()

instance ConnSrc IO where
  withConn conn fn = do
    conn' <- liftIO conn
    fn conn'
  closeConn _ = liftIO . sednaCloseConnection
