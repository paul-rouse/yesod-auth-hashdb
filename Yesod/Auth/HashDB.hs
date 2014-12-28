{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Auth.HashDB
-- Copyright   :  (c) Patrick Brisbin 2010, Paul Rouse 2014
-- License     :  MIT
--
-- Maintainer  :  Paul Rouse <pyr@doynton.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- A yesod-auth AuthPlugin designed to look users up in a Persistent
-- database where the hash of their password is stored.
--
-- This module was removed from @yesod-auth-1.3.0.0@ and is now
-- maintained separately.
-- Versions of this module prior to @yesod-auth-1.3@ used a relatively weak
-- hashing algorithm (a single round of SHA1) which does not provide
-- adequate protection against an attacker who discovers the hashed passwords.
-- See: <https://github.com/yesodweb/yesod/issues/668>.
--
-- It has now been rewritten to use "Crypto.PasswordStore", but this has been
-- done in a way which preserves compatibility both with the API and
-- with databases which have been set up using older versions of this module.
-- There are two levels of database compatibility:
--
-- * The verification code recognises both the old and new hash formats,
--   so passwords can be verified against database entries which still
--   contain old-style hashes.
--
-- * The function 'upgradePasswordHash' can be used to migrate
--   existing user records to use the new format hash.  Unlike
--   freshly created password hashes, entries converted this way
--   must still have the old salt field, since the old hash function
--   remains part of the algorithm needed for verification.  (The
--   new hash is layered on top of the old one.)
--
-- On the other hand, new passwords set up by 'setPassword' or
-- 'setPasswordStrength' no longer use a separate salt field, so new users
-- of this module need only provide a single password field in the user data,
-- and can ignore the salt.
--
-- In a system which has been migrated from the old format, passwords
-- which are reset will use the new format and will have an empty salt field.
-- Once all the entries are of this form, it is safe to change the model
-- to remove the salt, and change the 'HashDBUser' instance accordingly.
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
-- Create an instance of 'HashDBUser' for this data type.  For historical
-- reasons "Yesod.Auth.HashDB" exports some names which are quite likely to
-- clash with your own, so it is a good idea to import just the ones you need:
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
-- > import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)
-- > ....
-- > instance YesodAuth App where
-- >     ....
-- >     authPlugins _ = [ authHashDB (Just . UniqueUser), .... ]
-- >     getAuthId = getAuthIdHashDB AuthR (Just . UniqueUser)  -- Optional, see below
--
-- @AuthR@ should be your authentication route, and the function
-- @(Just . UniqueUser)@ supplied to both 'authHashDB' and
-- 'getAuthIdHashDB' takes a 'Text' and produces a 'Unique' value to
-- look up in the User table.  In a scaffolded site you may not need to
-- change the definition of @getAuthId@ at all, or you may prefer to modify
-- the function which the scaffolding defines: 'getAuthIdHashDB' is just a
-- convenience for the case when 'HashDB' is the only plugin.
--
-- The application developer should provide an interface for setting passwords;
-- it needs to call 'setPassword' and save the result in the database.
-- You can also create password hashes manually as follows, if you need to
-- initialise the database by hand:
--
-- > ghci -XOverloadedStrings
-- > > import Crypto.PasswordStore
-- > > makePassword "MyPassword" 14
--
-- where \"14\" is the default strength parameter ('defaultStrength') used
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
-------------------------------------------------------------------------------
module Yesod.Auth.HashDB
    ( HashDBUser(..)
    , Unique (..)
    , defaultStrength
    , setPasswordStrength
    , setPassword
    , validatePass
    , upgradePasswordHash
      -- * Interface to database and Yesod.Auth
    , validateUser
    , authHashDB
    , authHashDBWithForm
    , getAuthIdHashDB
      -- * Predefined data type
    , User
    , UserGeneric (..)
    , UserId
    , EntityField (..)
    , migrateUsers
    ) where

import Yesod.Persist
import Yesod.Form
import Yesod.Auth
import Yesod.Auth.Message          (AuthMessage(InvalidUsernamePass))
import Yesod.Core

import Control.Applicative         ((<$>), (<*>))
import Data.Typeable

import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Crypto.Hash as CH (SHA1, Digest, hash)
import Data.Text                   (Text, pack, unpack, append)
import Data.Maybe                  (fromMaybe)
import Crypto.PasswordStore        (makePassword, verifyPassword,
                                    passwordStrength, strengthenPassword)

-- | Default strength used for passwords (see "Crypto.PasswordStore" for
--   details).
defaultStrength :: Int
defaultStrength = 15

-- | The type representing user information stored in the database should
--   be an instance of this class.  It just provides the getters and setters
--   used by the functions in this module.
class HashDBUser user where
  -- | Retrieve password hash from user data
  userPasswordHash :: user -> Maybe Text
  -- | Retrieve salt for password from user data.  This is needed only for
  --   compatibility with old database entries, which contain the salt
  --   as a separate field.  New implementations do not require a separate
  --   salt field in the user data, and should leave this as the default.
  userPasswordSalt :: user -> Maybe Text
  userPasswordSalt _ = Just ""

  -- | Callback for 'setPassword' and 'upgradePasswordHash'.  Produces a
  --   version of the user data with the hash set to the new value.
  --
  --   This is the method which you should define for new applications, which
  --   do not require compatibility with databases containing hashes written
  --   by previous versions of this module.  If you do need compatibility,
  --   define 'setSaltAndPasswordHash' instead.
  setPasswordHash :: Text   -- ^ Password hash
                     -> user -> user
  setPasswordHash = setSaltAndPasswordHash ""

  setUserHashAndSalt :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setUserHashAndSalt =
      error "Define setSaltAndPasswordHash to get old-database compatibility"

  -- | Callback used in 'upgradePasswordHash' when compatibility is needed
  --   with old-style hashes (including ones already upgraded using
  --   'upgradePasswordHash').  This is not required for new applications,
  --   which do not have a separate salt field in user data: please define
  --   'setPasswordHash' instead.
  --
  --   The default implementation produces a runtime error, and will only be
  --   called if a non-empty salt value needs to be set for compatibility
  --   with an old database.
  setSaltAndPasswordHash :: Text    -- ^ Salt
                     -> Text    -- ^ Password hash
                     -> user -> user
  setSaltAndPasswordHash = setUserHashAndSalt

{-# MINIMAL userPasswordHash, (setPasswordHash | (userPasswordSalt, setSaltAndPasswordHash)) #-}
{-# DEPRECATED userPasswordSalt "Compatibility with old data containing a separate salt field will be removed eventually" #-}
{-# DEPRECATED setUserHashAndSalt "Please use setSaltAndPasswordHash instead" #-}
{-# DEPRECATED setSaltAndPasswordHash "Compatibility with old data containing a separate salt field will be removed eventually" #-}


-- | Calculate salted hash using SHA1.  Retained for compatibility with
--   hashes in existing databases, but will not be used for new passwords.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt pw =
  pack $ show (CH.hash $ BS.pack $ unpack $ append salt pw :: CH.Digest CH.SHA1)

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
--   This function retains compatibility with user data produced by old
--   versions of this module (prior to 1.3), although the hashes are less
--   secure and should be upgraded as soon as possible.  They can be
--   upgraded using 'upgradePasswordHash', or by insisting that users set
--   new passwords.
--
--   The result distinguishes two types of validation failure, which may
--   be useful in an application which supports multiple authentication
--   methods:
--
--   * Just False - the user has a password set up, but the given one does
--     not match it
--
--   * Nothing - the user does not have a password (the hash is Nothing)
--
--   Since 1.4.1
--
validatePass :: HashDBUser u => u -> Text -> Maybe Bool
validatePass user passwd = do
    hash <- userPasswordHash user
    salt <- userPasswordSalt user
    -- NB plaintext password characters are truncated to 8 bits here, and also
    -- in saltedHash and passwordHash above (the hash and old salt are already
    -- 8 bit).  This is for historical compatibility, but in practice it is
    -- unlikely to reduce the entropy of most users' alphabets by much.
    let hash' = BS.pack $ unpack hash
        passwd' = BS.pack $ unpack $ if salt == "" then passwd
                                     else
                                         -- Extra layer for an upgraded old hash
                                         saltedHash salt passwd
    if passwordStrength hash' > 0
        -- Will give >0 for new-style hash, else fall back
        then return $ verifyPassword passwd' hash'
        else return $ hash == saltedHash salt passwd

-- | Upgrade existing user credentials to a stronger hash.  The existing
--   hash may have been produced either by previous versions of this module,
--   which used a weak algorithm, or from a weaker setting in the current
--   algorithm.  Use this function to produce an updated user record to
--   store in the database.
--
--   To allow transitional use, starting from hashes produced by older
--   versions of this module, and upgrading them to the new format,
--   we have to use the hash alone, without knowledge of the user's
--   plaintext password.  In this case, we apply the new algorithm to the
--   old hash, resulting in both hash functions, old and new, being used
--   one on top of the other; this situation is recognised by the hash
--   having the new format while the separate salt field is non-empty.
--
--   Returns Nothing if the user has no password (ie if 'userPasswordHash' u
--   is 'Nothing' and/or 'userPasswordSalt' u is 'Nothing').
upgradePasswordHash :: (MonadIO m, HashDBUser user) => Int -> user -> m (Maybe user)
upgradePasswordHash strength u = do
    let old = do h <- userPasswordHash u
                 s <- userPasswordSalt u
                 return (h, s)
    case old of
        Just (oldHash, oldSalt) -> do
            let oldHash' = BS.pack $ unpack oldHash
            if passwordStrength oldHash' > 0
              then
                -- Already a new-style hash, so only strengthen it as needed
                let newHash = pack $ BS.unpack $ strengthenPassword oldHash' strength
                in if oldSalt == ""
                   then return $ Just $ setPasswordHash newHash u
                   else return $ Just $ setSaltAndPasswordHash oldSalt newHash u
              else do
                -- Old-style hash: do extra layer of hash with the new algorithm
                newHash <- passwordHash strength oldHash
                return $ Just $ setSaltAndPasswordHash oldSalt newHash u
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
    , AuthId master ~ Key user
    , PersistEntityBackend user ~ YesodPersistBackend master
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
        else loginErrorMessageI LoginR InvalidUsernamePass

-- | A drop in for the getAuthId method of your YesodAuth instance which
--   can be used if authHashDB is the only plugin in use.
getAuthIdHashDB :: HashDBPersist site user =>
                   (AuthRoute -> Route site)     -- ^ your site's Auth Route
                -> (Text -> Maybe (Unique user)) -- ^ gets user ID
                -> Creds site                    -- ^ the creds argument
                -> HandlerT site IO (Maybe (AuthId site))
getAuthIdHashDB authR uniq creds = do
    muid <- maybeAuthId
    case muid of
        -- user already authenticated
        Just uid -> return $ Just uid
        Nothing       -> do
            x <- case uniq (credsIdent creds) of
                   Nothing -> return Nothing
                   Just u  -> runDB (getBy u)
            case x of
                -- user exists
                Just (Entity uid _) -> return $ Just uid
                Nothing       -> do
                    _ <- loginErrorMessage (authR LoginR) "Invalid username/password."
                    return Nothing

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
defaultForm loginRoute = toWidget [hamlet|
$newline never
    <div id="header">
        <h1>Login

    <div id="login">
        <form method="post" action="@{loginRoute}">
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


----------------------------------------------------------------
-- Predefined datatype
----------------------------------------------------------------

-- | Generate data base instances for a valid user
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateUsers"]
         [persistUpperCase|
User
    username Text Eq
    password Text
    salt     Text
    UniqueUser username
    deriving Typeable
|]
{-# DEPRECATED User, migrateUsers "The predefined User data type will be removed soon - please define your own database table and accompanying instance of HashDBUser" #-}

instance HashDBUser (UserGeneric backend) where
  userPasswordHash = Just . userPassword
  userPasswordSalt = Just . userSalt
  setSaltAndPasswordHash s h u = u { userSalt     = s
                               , userPassword = h
                               }
