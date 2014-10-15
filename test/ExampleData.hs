{-# LANGUAGE OverloadedStrings #-}
module ExampleData (
    OldStyleUser (..),
    NewStyleUser (..),
    mypassword,
    changedpw,
    equivpassword,
    equivchangedpw,
    oldStyleValidUser,
    oldStyleBadUser1,
    oldStyleBadUser2,
    newStyleValidUser,
    newStyleBadUser,
    stronger
) where

import Yesod.Auth.HashDB (HashDBUser(..), defaultStrength)
import Data.Text (Text)

data OldStyleUser = OldStyleUser {
                        oldStyleName :: Text,
                        oldStylePass :: Maybe Text,
                        oldStyleSalt :: Maybe Text
                    } deriving (Eq, Show)

instance HashDBUser OldStyleUser where
    userPasswordHash = oldStylePass
    userPasswordSalt = oldStyleSalt
    setSaltAndPasswordHash s h u = u { oldStyleSalt = Just s,
                                       oldStylePass = Just h
                                     }

data NewStyleUser = NewStyleUser {
                        newStyleName :: Text,
                        newStylePass :: Maybe Text
                    } deriving (Eq, Show)

instance HashDBUser NewStyleUser where
    userPasswordHash = newStylePass
    setPasswordHash h u = u { newStylePass = Just h }

mypassword :: Text
mypassword = "mypassword"
changedpw :: Text
changedpw = "changedpw"

-- These are equivalent to the above if each character is truncated to 8 bits
equivpassword :: Text
equivpassword = "\x46d\x479\x470\x461\x473\x473\x477\x46f\x472\x464"
equivchangedpw :: Text
equivchangedpw = "\xbc63\xbc68\xbc61\xbc6e\xbc67\xbc65\xbc64\xbc70\xbc77"

oldStyleValidUser :: OldStyleUser
oldStyleValidUser =
    OldStyleUser "foo"
                 (Just "8e3e33029e71b4e25ba95a00a88c4bfeb93d766a")
                 (Just "somesalt")

oldStyleBadUser1 :: OldStyleUser
oldStyleBadUser1 =
    OldStyleUser "bar"
                 Nothing
                 (Just "pepper")

oldStyleBadUser2 :: OldStyleUser
oldStyleBadUser2 =
    OldStyleUser "baz"
                 (Just "8e3e33029e71b4e25ba95a00a88c4bfeb93d766a")
                 Nothing

newStyleValidUser :: NewStyleUser
newStyleValidUser =
    NewStyleUser "fox"
                 (Just "sha256|14|2hL7cNopkA/dGy/5CQTuSg==|CUTPW6ICMISSjohFep851f9PdqIn7Y4B75/I77BvEYM=")

newStyleBadUser :: NewStyleUser
newStyleBadUser =
    NewStyleUser "bad"
                 Nothing

stronger :: Int
stronger = defaultStrength + 2
