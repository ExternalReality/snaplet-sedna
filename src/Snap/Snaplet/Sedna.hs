{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Snap.Snaplet.Sedna where


-------------------------------------------------------------------------------
import           Control.Monad.IO.Control
import           Control.Monad.State
import           Database.SednaDB.SednaTypes
import qualified Database.SednaDB.SednaBindings as Sedna 
import           Snap.Snaplet
import           Snap.Snaplet.Sedna.Types


-------------------------------------------------------------------------------
class (MonadControlIO m, ConnSrc s) => HasSedna m s | m -> s where
  getConnSrc :: m (s SednaConnection)


-------------------------------------------------------------------------------
data SednaSnaplet s  =
    ConnSrc s => SednaSnaplet { connSrc :: s SednaConnection }


-------------------------------------------------------------------------------
instance MonadControlIO (Handler b v) where
  liftControlIO f = liftIO (f return)


-------------------------------------------------------------------------------
sednaInit :: ConnSrc s => (s SednaConnection) -> SnapletInit b (SednaSnaplet s)
sednaInit src = makeSnaplet "sedna" "Sedna Database Connectivity" Nothing $ do 
                  return $ SednaSnaplet src


-------------------------------------------------------------------------------
withSedna :: (MonadControlIO m, HasSedna m s) => (SednaConnection -> IO a) -> m a
withSedna f = do
  src <- getConnSrc
  liftIO $ withConn src (liftIO . f)


-------------------------------------------------------------------------------
withSedna' :: HasSedna m s => (SednaConnection -> a) -> m a
withSedna' f = do
  src <- getConnSrc
  liftIO $ withConn src (return . f)


-------------------------------------------------------------------------------
query :: HasSedna m s => Query -> m QueryResult
query xQuery = do  
  withSedna (`Sedna.sednaExecute` xQuery)
  withSedna Sedna.sednaGetResultString 


-------------------------------------------------------------------------------
disconnect :: HasSedna m s => m ()
disconnect = withSedna Sedna.sednaCloseConnection                 
         

------------------------------------------------------------------------------- 
commit :: HasSedna m s => m ()
commit = withSedna Sedna.sednaCommit


------------------------------------------------------------------------------- 
rollback :: HasSedna m s => m ()
rollback = withSedna Sedna.sednaRollBack


--------------------------------------------------------------------------------
--loadXMLFile :: HasSedna m s => Document -> Collection -> m ()
--loadXMLFile filepath doc coll = withSedna (`Sedna.sednaLoadFile` filepath doc coll)

--------------------------------------------------------------------------------

