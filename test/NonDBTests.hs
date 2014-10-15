{-# LANGUAGE OverloadedStrings #-}
module NonDBTests (
    nonDBTests
) where

import Test.Hspec
import Yesod.Auth.HashDB
import Data.Text (pack, isInfixOf)

import ExampleData


nonDBTests :: Spec
nonDBTests = do

    describe "validatePass" $ do

      context "Old-style valid user" $ do
        it "verifies OK with correct password" $
            validatePass oldStyleValidUser mypassword `shouldBe` Just True
        it "fails validation with a wrong password giving Just False" $
            validatePass oldStyleValidUser changedpw `shouldBe` Just False

      context "Old-style user with no password hash" $ do
        it "fails validation giving Nothing" $
            validatePass oldStyleBadUser1 mypassword `shouldBe` Nothing

      context "Old-style user with no salt" $ do
        it "fails validation giving Nothing" $
            validatePass oldStyleBadUser2 mypassword `shouldBe` Nothing

      context "New style valid user" $ do
        it "verifies OK with correct password" $
            validatePass newStyleValidUser mypassword `shouldBe` Just True
        it "fails validation with a wrong password giving Just False" $
            validatePass newStyleValidUser changedpw `shouldBe` Just False

      context "New-style user with no password hash" $ do
        it "fails validation giving Nothing" $
            validatePass newStyleBadUser mypassword `shouldBe` Nothing


    describe "upgradePasswordHash" $ do

      context "Upgrade of old-style password hash" $ do
        it "has the same salt value" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleValidUser
            let newsalt = newuser >>= userPasswordSalt
            newsalt `shouldBe` Just "somesalt"
        it "still verifies with the same password" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleValidUser
            let valid = newuser >>= flip validatePass mypassword
            valid `shouldBe` Just True
        it "still works after a second upgrade to a stronger setting" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleValidUser
            neweruser <- case newuser of
                           Just u -> upgradePasswordHash stronger u
                           Nothing -> return $ Just oldStyleBadUser1 -- failure
            let valid = neweruser >>= flip validatePass mypassword
            valid `shouldBe` Just True
        it "is Nothing if there is no password hash" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleBadUser1
            newuser `shouldBe` Nothing
        it "is Nothing if there is no salt" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleBadUser2
            newuser `shouldBe` Nothing

      context "Upgrade of new-style password hash to stronger setting" $ do
        it "really does have a hash containing the new strength" $ do
            newuser <- upgradePasswordHash stronger newStyleValidUser
            let s = pack $ "|" ++ show stronger ++ "|"
                found = fmap (s `isInfixOf`) $ newuser >>= newStylePass
            found `shouldBe` Just True
        it "still verifies with the same password" $ do
            newuser <- upgradePasswordHash stronger newStyleValidUser
            let valid = newuser >>= flip validatePass mypassword
            valid `shouldBe` Just True
        it "is Nothing if there is no password hash" $ do
            newuser <- upgradePasswordHash stronger newStyleBadUser
            newuser `shouldBe` Nothing


    describe "setPassword" $ do
        it "produces hash which verifies OK starting from old-style user" $ do
            newuser <- setPassword changedpw oldStyleValidUser
            validatePass newuser changedpw `shouldBe` Just True
        it "produces empty salt (in the case of old-style user)" $ do
            newuser <- setPassword changedpw oldStyleValidUser
            userPasswordSalt newuser `shouldBe` Just ""
        it "produces hash which verifies OK starting from new-style user" $ do
            newuser <- setPassword changedpw newStyleValidUser
            validatePass newuser changedpw `shouldBe` Just True


    describe "Only the bottom 8 bits of password characters are used" $ do
        it "when verifying old-style password hash" $
            validatePass oldStyleValidUser equivpassword `shouldBe` Just True
        it "when verifying new-style password hash" $
            validatePass newStyleValidUser equivpassword `shouldBe` Just True
        it "when setting a new password" $ do
            newuser <- setPassword equivchangedpw oldStyleValidUser
            validatePass newuser changedpw `shouldBe` Just True
