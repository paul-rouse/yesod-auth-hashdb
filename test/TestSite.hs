{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveDataTypeable         #-}
#endif
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TestSite (
    User(..),
    migrateAll,
    App(..),
    Route(..),
    Handler,
    runDB
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative          ((<$>))
import Data.Typeable                (Typeable)
#endif
import Control.Monad                (when)
import Data.Text
import Database.Persist.Sqlite
import Network.HTTP.Client.Conduit  (Manager)
import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB            (HashDBUser(..), authHashDB,
                                     submitRouteHashDB)
import Yesod.Auth.Message           (AuthMessage (InvalidLogin))

#if ! MIN_VERSION_yesod_auth(1,4,9)
import Yesod.Auth.Message           (AuthMessage (LoginTitle))
defaultLoginHandler :: AuthHandler master Html
defaultLoginHandler = do
    tp <- getRouteToParent
    lift $ authLayout $ do
        setTitleI LoginTitle
        master <- getYesod
        mapM_ (flip apLogin tp) (authPlugins master)
#endif


-- Trivial example site needing authentication
--
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name       Text
    password   Text Maybe
    UniqueUser name
    deriving Show
#if __GLASGOW_HASKELL__ < 710
    deriving Typeable
#endif
|]

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

data App = App
    { appHttpManager  :: Manager
    , appDBConnection :: SqlBackend
    }

mkYesod "App" [parseRoutes|
/       HomeR GET
/prot   ProtectedR GET
/auth   AuthR Auth getAuth
|]

instance Yesod App where
    approot = ApprootStatic "http://localhost:3000"

    authRoute _ = Just $ AuthR LoginR

    isAuthorized ProtectedR _ = do
        mu <- maybeAuthId
        return $ case mu of
            Nothing -> AuthenticationRequired
            Just _  -> Authorized
    -- Other pages (HomeR and AuthR _) do not require login
    isAuthorized _ _ = return Authorized

#if MIN_VERSION_yesod_core(1,4,19) || (MIN_VERSION_yesod_core(1,4,14) && MIN_VERSION_yesod_test(1,4,4))
    -- CSRF middleware requires yesod-core-1.4.14, but only include it if tests
    -- can get at the token (see IntegrationTest.hs for additional comment).
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
#endif

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlConn action $ appDBConnection master

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing             -> return $ UserError InvalidLogin

    authPlugins _ = [ authHashDB (Just . UniqueUser) ]

    authHttpManager = appHttpManager

    loginHandler = do
        submission <- submitRouteHashDB
        render <- lift getUrlRender
        dflt <- defaultLoginHandler
        typedContent@(TypedContent ct _) <- selectRep $ do
            provideRep $ return dflt
            provideJson $ object [("loginUrl", toJSON $ render submission)]
        when (ct == typeJson) $
            -- Short-circuit JSON response, but must still "return" Html
            -- below to get correct type for loginHandler
            sendResponse typedContent
        return dflt

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    mauth <- maybeAuth
    let mname = userName . entityVal <$> mauth
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show mname}
            $maybe _ <- mname
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
            <p><a href=@{ProtectedR}>Go to protected page
        |]

-- This page requires a valid login
getProtectedR :: Handler Html
getProtectedR = defaultLayout
        [whamlet|
            <p>OK, you are logged in so you are allowed to see this!
            <p><a href=@{HomeR}>Go to home page
        |]
