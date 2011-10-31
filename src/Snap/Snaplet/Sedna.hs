module Snap.Snaplet.Sedna where
-------------------------------------------------------------------------------
import           Data.Pool
import           Database.SednaDB.SednaTypes
import qualified Database.SednaDB.SednaBindings as Sedna 
import           Snap.Snaplet


-------------------------------------------------------------------------------
data SednaSnaplet = SednaSnaplet { sednaConnectionPool :: Pool SednaConnection }


-------------------------------------------------------------------------------
sednaInit conn = sednaInit' $ createPool  conn 
                                          Sedna.sednaCloseConnection 
                                          1 
                                          300 
                                          1  
