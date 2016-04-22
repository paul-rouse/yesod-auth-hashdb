{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger         (runStderrLoggingT)
import Database.Persist.Sqlite
import Network.HTTP.Client.Conduit  (newManager)
import Test.Hspec                   (hspec)
import Yesod
import Yesod.Auth.HashDB            (setPassword)

import IntegrationTest
import TestSite

main :: IO ()
main = runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> liftIO $ do
    runResourceT $ flip runSqlConn conn $ do
        runMigration migrateAll
        validUser <- setPassword "MyPassword" $ User "paul" Nothing
        insert_ validUser
    mgr <- newManager
    -- To try this as a server, replace the `hspec` line below with this one,
    -- and remove the import of IntegrationTest above.
    -- warp 3000 $ App mgr conn
    hspec $ withApp (App mgr conn) integrationSpec
