{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Auth.HashDB
-- Copyright   :  (c) Patrick Brisbin 2010, Paul Rouse 2014-2016
-- License     :  MIT
--
-- Maintainer  :  Paul Rouse <pyr@doynton.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- A Yesod authentication plugin designed to look users up in a Persistent
-- database where the hash of their password is stored.
--
-- __Releases 1.6 finishes the process of removing compatibility with old__
-- __(pre 1.3) databases.  Please see__
-- __<https://github.com/paul-rouse/yesod-auth-hashdb/blob/master/Upgrading.md>__
--
-- To use this in a Yesod application, the foundation data type must be an
-- instance of YesodPersist, and the username and hashed passwords should
-- be added to the database.  The following steps give an outline of what
-- is required.
--
-- You need a database table to store user records: in a scaffolded site it
-- might look like:
--
-- > User
-- >     name Text             -- user name used to uniquely identify users
-- >     password Text Maybe   -- password hash for HashDB
-- >     UniqueUser name
--
-- Create an instance of 'HashDBUser' for this data type:
--
-- > import Yesod.Auth.HashDB (HashDBUser(..))
-- > ....
-- > instance HashDBUser User where
-- >     userPasswordHash = userPassword
-- >     setPasswordHash h u = u { userPassword = Just h }
--
-- In the YesodAuth instance declaration for your app, include 'authHashDB'
-- like so:
--
-- > import Yesod.Auth.HashDB (authHashDB)
-- > ....
-- > instance YesodAuth App where
-- >     ....
-- >     authPlugins _ = [ authHashDB (Just . UniqueUser), .... ]
-- >     getAuthId creds = ... -- Perhaps modify scaffolding: see below
--
-- The argument to 'authHashDB' is a function which takes a 'Text' and
-- produces a 'Maybe' containing a 'Unique' value to look up in the User
-- table.  The example @(Just . UniqueUser)@ shown here works for the
-- model outlined above.
--
-- In the scaffolding, the definition of @getAuthId@ contains code to
-- add a user who is not already in the database.  Depending on how users
-- are administered, this may not make sense when using HashDB, so consider
-- whether it should be removed.
--
-- For a real application, the developer should provide some sort of
-- of administrative interface for setting passwords; it needs to call
-- 'setPassword' and save the result in the database.  However, if you
-- need to initialise the database by hand, you can generate the correct
-- password hash as follows:
--
-- > ghci -XOverloadedStrings
-- > > import Crypto.PasswordStore
-- > > makePassword "MyPassword" 17
--
-- where \"17\" is the default strength parameter ('defaultStrength') used
-- in this module.
--
-- == Custom Login Form
--
-- Instead of using the built-in HTML form, a custom one can be supplied
-- by using 'authHashDBWithForm' instead of 'authHashDB'.
--
-- The custom form needs to be given as a function returning a Widget, since
-- it has to build in the supplied "action" URL, and it must provide two text
-- fields called "username" and "password".  For example, the following
-- modification of the outline code given above would replace the default
-- form with a very minimal one which has no labels and a simple layout.
--
-- > instance YesodAuth App where
-- >     ....
-- >     authPlugins _ = [ authHashDBWithForm myform (Just . UniqueUser), .... ]
-- >
-- > myform :: Route App -> Widget
-- > myform action = $(whamletFile "templates/loginform.hamlet")
--
-- where templates/loginform.hamlet contains
--
-- > <form method="post" action="@{action}">
-- >     <input name="username">
-- >     <input type="password" name="password">
-- >     <input type="submit" value="Login">
--
-- If a CSRF token needs to be embedded in a custom form, code must be
-- included in the widget to add it - see @defaultForm@ in the source
-- code of this module for an example.
--
-------------------------------------------------------------------------------
module Yesod.Auth.HashDB
    ( HashDBUser(..)
    , defaultStrength
    , setPasswordStrength
    , setPassword
    , validatePass
    , upgradePasswordHash
      -- * Interface to database and Yesod.Auth
    , validateUser
    , authHashDB
    , authHashDBWithForm
    ) where


#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative         ((<$>), (<*>))
#endif
import           Crypto.PasswordStore        (makePassword, strengthenPassword,
                                              verifyPassword, passwordStrength)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack, unpack)
import           Yesod.Auth
import qualified Yesod.Auth.Message    as Msg
import           Yesod.Core
import           Yesod.Form
import           Yesod.Persist

#if !MIN_VERSION_yesod_core(1,4,14)
defaultCsrfParamName :: Text
defaultCsrfParamName = "_token"
#endif

-- | Default strength used for passwords (see "Crypto.PasswordStore" for
--   details).
defaultStrength :: Int
defaultStrength = 17

-- | The type representing user information stored in the database should
--   be an instance of this class.  It just provides the getter and setter
--   used by the functions in this module.
class HashDBUser user where
    -- | Getter used by 'validatePass' and 'upgradePasswordHash' to
    --   retrieve the password hash from user data
    --
    userPasswordHash :: user -> Maybe Text

    -- | Setter used by 'setPassword' and 'upgradePasswordHash'.  Produces a
    --   version of the user data with the hash set to the new value.
    --
    setPasswordHash :: Text   -- ^ Password hash
                       -> user -> user

    {-# MINIMAL userPasswordHash, setPasswordHash #-}


-- | Calculate a new-style password hash using "Crypto.PasswordStore".
passwordHash :: MonadIO m => Int -> Text -> m Text
passwordHash strength pwd = do
    h <- liftIO $ makePassword (BS.pack $ unpack pwd) strength
    return $ pack $ BS.unpack h

-- | Set password for user, using the given strength setting. Use this
--   function, or 'setPassword', to produce a user record containing the
--   hashed password.  Unlike previous versions of this module, no separate
--   salt field is required for new passwords (but it may still be required
--   for compatibility while old password hashes remain in the database).
--
--   This function does not change the database; the calling application
--   is responsible for saving the data which is returned.
setPasswordStrength :: (MonadIO m, HashDBUser user) => Int -> Text -> user -> m user
setPasswordStrength strength pwd u = do
    hashed <- passwordHash strength pwd
    return $ setPasswordHash hashed u

-- | As 'setPasswordStrength', but using the 'defaultStrength'
setPassword :: (MonadIO m, HashDBUser user) => Text -> user -> m user
setPassword = setPasswordStrength defaultStrength

-- | Validate a plaintext password against the hash in the user data structure.
--
--   The result distinguishes two types of validation failure, which may
--   be useful in an application which supports multiple authentication
--   methods:
--
--   * Just False - the user has a password set up, but the given one does
--     not match it
--
--   * Nothing - the user does not have a password ('userPasswordHash' returns
--     Nothing)
--
--   Since 1.4.1
--
validatePass :: HashDBUser u => u -> Text -> Maybe Bool
validatePass user passwd = do
    hash <- userPasswordHash user
    -- NB plaintext password characters are truncated to 8 bits here,
    -- and also in passwordHash above (the hash is already 8 bit).
    -- This is for historical compatibility, but in practice it is
    -- unlikely to reduce the entropy of most users' alphabets by much.
    let hash' = BS.pack $ unpack hash
        passwd' = BS.pack $ unpack passwd
    if passwordStrength hash' > 0
        -- Will give >0 for valid hash format, else treat as if wrong password
        then return $ verifyPassword passwd' hash'
        else return False

-- | Upgrade existing user credentials to a stronger hash.  The existing
--   hash will have been produced from a weaker setting in the current
--   algorithm.  Use this function to produce an updated user record to
--   store in the database.
--
--   As of version 1.5 this function cannot be used to upgrade a hash
--   which has a non-empty separate salt field.  Such entries would have
--   been produced originally by versions of this module prior to 1.3,
--   but may have been upgraded using earlier versions of this function.
--
--   Returns Nothing if the user has no password (ie if 'userPasswordHash' u
--   is 'Nothing') or if the password hash is not in the correct format.
--
upgradePasswordHash :: (MonadIO m, HashDBUser user) => Int -> user -> m (Maybe user)
upgradePasswordHash strength u = do
    let old = userPasswordHash u
    case old of
        Just oldHash -> do
            let oldHash' = BS.pack $ unpack oldHash
            if passwordStrength oldHash' > 0
              then
                -- Valid hash format, so strengthen it as needed
                let newHash = pack $ BS.unpack $ strengthenPassword oldHash' strength
                in return $ Just $ setPasswordHash newHash u
              else do
                -- Invalid hash format (perhaps from old version of this module)
                return Nothing
        Nothing -> return Nothing


----------------------------------------------------------------
-- Interface to database and Yesod.Auth
----------------------------------------------------------------

-- | Constraint for types of interface functions in this module
--
type HashDBPersist master user =
    ( YesodAuthPersist master
    , PersistUnique (YesodPersistBackend master)
    , AuthEntity master ~ user
#if MIN_VERSION_persistent(2,5,0)
    , PersistEntityBackend user ~ BaseBackend (YesodPersistBackend master)
#else
    , PersistEntityBackend user ~ YesodPersistBackend master
#endif
    , HashDBUser user
    , PersistEntity user
    )

-- | Given a user ID and password in plaintext, validate them against
--   the database values.  This function simply looks up the user id in the
--   database and calls 'validatePass' to do the work.
--
validateUser :: HashDBPersist site user =>
                Unique user     -- ^ User unique identifier
             -> Text            -- ^ Password in plaintext
             -> HandlerT site IO Bool
validateUser userID passwd = do
    -- Get user data
    user <- runDB $ getBy userID
    return $ fromMaybe False $ flip validatePass passwd . entityVal =<< user


login :: AuthRoute
login = PluginR "hashdb" ["login"]


-- | Handle the login form. First parameter is function which maps
--   username (whatever it might be) to unique user ID.
postLoginR :: HashDBPersist site user =>
              (Text -> Maybe (Unique user))
           -> HandlerT Auth (HandlerT site IO) TypedContent
postLoginR uniq = do
    (mu,mp) <- lift $ runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    isValid <- lift $ fromMaybe (return False) 
                 (validateUser <$> (uniq =<< mu) <*> mp)
    if isValid 
        then lift $ setCredsRedirect $ Creds "hashdb" (fromMaybe "" mu) []
        else loginErrorMessageI LoginR Msg.InvalidUsernamePass


-- | Prompt for username and password, validate that against a database
--   which holds the username and a hash of the password
authHashDB :: HashDBPersist site user =>
              (Text -> Maybe (Unique user)) -> AuthPlugin site
authHashDB = authHashDBWithForm defaultForm


-- | Like 'authHashDB', but with an extra parameter to supply a custom HTML
-- form.
--
-- The custom form should be specified as a function which takes a route to
-- use as the form action, and returns a Widget containing the form.  The
-- form must use the supplied route as its action URL, and, when submitted,
-- it must send two text fields called "username" and "password".
--
-- Please see the example in the documentation at the head of this module.
--
-- Since 1.3.2
--
authHashDBWithForm :: HashDBPersist site user =>
                      (Route site -> WidgetT site IO ())
                   -> (Text -> Maybe (Unique user))
                   -> AuthPlugin site
authHashDBWithForm form uniq =
    AuthPlugin "hashdb" dispatch $ \tm -> form (tm login)
    where
        dispatch "POST" ["login"] = postLoginR uniq >>= sendResponse
        dispatch _ _              = notFound


defaultForm :: Yesod app => Route app -> WidgetT app IO ()
defaultForm loginRoute = do
    request <- getRequest
    let mtok = reqToken request
    toWidget [hamlet|
      $newline never
      <div id="header">
        <h1>Login

      <div id="login">
        <form method="post" action="@{loginRoute}">
          $maybe tok <- mtok
            <input type=hidden name=#{defaultCsrfParamName} value=#{tok}>
          <table>
            <tr>
              <th>Username:
              <td>
                <input id="x" name="username" autofocus="" required>
            <tr>
              <th>Password:
              <td>
                <input type="password" name="password" required>
            <tr>
              <td>&nbsp;
              <td>
                <input type="submit" value="Login">

          <script>
            if (!("autofocus" in document.createElement("input"))) {
                document.getElementById("x").focus();
            }

    |]
