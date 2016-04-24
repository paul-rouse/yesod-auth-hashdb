{-# LANGUAGE CPP                        #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module IntegrationTest (
    withApp,
    integrationSpec
) where

import ClassyPrelude
import Test.Hspec                   (Spec, SpecWith, before,
                                     describe, context, it)
import qualified Yesod.Test as YT

import TestSite                     (App, Route(..), needCSRFToken)
import TestTools

#if MIN_VERSION_yesod_test(1,5,0)
type MyTestApp = YT.TestApp App
withApp :: App -> SpecWith (YT.TestApp App) -> Spec
withApp app = before $ return (app, id)
#else
type MyTestApp = App
withApp :: App -> SpecWith App -> Spec
withApp app = before $ return app
#endif


integrationSpec :: SpecWith MyTestApp
integrationSpec = do
    describe "The home page" $ do
      it "can be accessed" $ do
        YT.get HomeR
        YT.statusIs 200

    describe "The protected page" $ do
      it "requires login" $ do
        needsLogin GET ("/prot" :: Text)
      it "looks right after login by a valid user" $ do
        _ <- doLogin "paul" "MyPassword"
        YT.get ProtectedR
        YT.statusIs 200
        YT.bodyContains "OK, you are logged in so you are allowed to see this!"
      it "can't be accessed after login then logout" $ do
        _ <- doLogin "paul" "MyPassword"
        YT.get $ AuthR LogoutR
        -- That `get` will get the form from Yesod.Core.Handler.redirectToPost
        -- which will not be submitted automatically without javascript
        YT.bodyContains "please click on the button below to be redirected"
        -- so we do the redirection ourselves:
        YT.request $ do
            YT.setMethod "POST"
            YT.setUrl $ AuthR LogoutR
            when needCSRFToken
                YT.addToken
        YT.get HomeR
        YT.statusIs 200
        YT.bodyContains "Your current auth ID: Nothing"
        YT.get ProtectedR
        YT.statusIs 303

    describe "Login" $ do
      it "fails when incorrect password given" $ do
        loc <- doLoginPart1 "paul" "WrongPassword"
        checkFailedLogin loc
      it "fails when unknown user name given" $ do
        loc <- doLoginPart1 "paul" "WrongPassword"
        checkFailedLogin loc
