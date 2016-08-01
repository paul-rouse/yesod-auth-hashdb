{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger         (runStderrLoggingT)
import Database.Persist.Sqlite
import Network.HTTP.Client.Conduit  (newManager)
import Test.Hspec                   (hspec)
import Yesod
import Yesod.Auth.HashDB            (setPassword)

#ifndef STANDALONE
import IntegrationTest
#endif
import TestSite

main :: IO ()
main = runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> liftIO $ do
    runResourceT $ flip runSqlConn conn $ do
        runMigration migrateAll
        validUser <- setPassword "MyPassword" $ User "paul" Nothing
        insert_ validUser
    mgr <- newManager
#ifdef STANDALONE
    -- Run as a stand-alone server - see the cabal file in this directory
    warp 3000 $ App mgr conn
#else
    -- Otherwise run the tests
    hspec $ withApp (App mgr conn) integrationSpec
#endif
