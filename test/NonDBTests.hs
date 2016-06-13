{-# LANGUAGE OverloadedStrings #-}
module NonDBTests (
    nonDBTests
) where

import Test.Hspec
import Yesod.Auth.HashDB
import Data.Text (pack, isInfixOf)

import ExampleData

-- For consistent layout of "it" items, we sometimes have a redundant "do":
{-# ANN nonDBTests ("HLint: ignore Redundant do"::String) #-}

nonDBTests :: Spec
nonDBTests = do

    describe "validatePass" $ do

      context "Naively upgraded old-style valid user" $ do
        it "no longer checks good password (wrong format, so Just False)" $
            validatePass oldStyleValidUser mypassword `shouldBe` Just False
        it "no longer checks bad password (wrong format, so Just False)" $
            validatePass oldStyleValidUser changedpw `shouldBe` Just False

      context "Naively upgraded old-style user with no password hash" $ do
        it "fails validation giving Nothing" $
            validatePass oldStyleBadUser1 mypassword `shouldBe` Nothing

      context "Naively upgraded old-style user with no salt" $ do
        it "fails validation (wrong format, so Just False)" $
            validatePass oldStyleBadUser2 mypassword `shouldBe` Just False

      context "Previously upgraded old-style user" $ do
        it "no longer verifies with correct password as it still needs salt" $
            validatePass oldStyleUpgradedUser mypassword `shouldBe` Just False
        it "fails validation with a wrong password giving Just False" $
            validatePass oldStyleUpgradedUser changedpw `shouldBe` Just False

      context "New style valid user" $ do
        it "verifies OK with correct password" $
            validatePass newStyleValidUser mypassword `shouldBe` Just True
        it "fails validation with a wrong password giving Just False" $
            validatePass newStyleValidUser changedpw `shouldBe` Just False

      context "New-style user with no password hash" $ do
        it "fails validation giving Nothing" $
            validatePass newStyleBadUser mypassword `shouldBe` Nothing


    describe "upgradePasswordHash" $ do

      context "Upgrade of naively upgraded old-style password hash" $ do
        it "is Nothing if the user has a password (format is wrong)" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleValidUser
            newuser `shouldBe` Nothing
        it "is Nothing if there is no password hash" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleBadUser1
            newuser `shouldBe` Nothing

      context "Upgrade of previously upgraded old-style user" $ do
        it "silently succeeds producing a password which won't verify as it still needs salt" $ do
            newuser <- upgradePasswordHash defaultStrength oldStyleUpgradedUser
            case newuser of
              Nothing -> newuser `shouldNotBe` Nothing
              Just u  -> validatePass u mypassword `shouldBe` Just False

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
            oldStyleSalt newuser `shouldBe` Just ""
        it "produces hash which verifies OK starting from new-style user" $ do
            newuser <- setPassword changedpw newStyleValidUser
            validatePass newuser changedpw `shouldBe` Just True


    describe "Only the bottom 8 bits of password characters are used" $ do
        it "when verifying new-style password hash" $
            validatePass newStyleValidUser equivpassword `shouldBe` Just True
        it "when setting a new password" $ do
            newuser <- setPassword equivchangedpw oldStyleValidUser
            validatePass newuser changedpw `shouldBe` Just True
