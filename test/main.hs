{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import NonDBTests

main :: IO ()
main = hspec nonDBTests
