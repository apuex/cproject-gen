{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main(main) where

import Util
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Util" $ do
        
        it "cToPascal `hello_world` should equals `HelloWorld`" $ do
            let expected = "HelloWorld"
            let input = "hello_world"
            (cToPascal input) `shouldBe` expected

        it "cToPascal `fuck_you_trump` should equals `FuckYouTrump`" $ do
            let expected = "FuckYouTrump"
            let input = "fuck_you_trump"
            (cToPascal input) `shouldBe` expected

        it "cToCamel `hello_world` should equals `helloWorld`" $ do
            let expected = "helloWorld"
            let input = "hello_world"
            (cToCamel input) `shouldBe` expected

        it "cToCamel `fuck_you_trump` should equals `fuckYouTrump`" $ do
            let expected = "fuckYouTrump"
            let input = "fuck_you_trump"
            (cToCamel input) `shouldBe` expected

        it "cToShell `hello_world` should equals `hello-world`" $ do
            let expected = "hello-world"
            let input = "hello_world"
            (cToShell input) `shouldBe` expected

        it "cToShell `fuck_you_trump` should equals `fuck-you-trump`" $ do
            let expected = "fuck-you-trump"
            let input = "fuck_you_trump"
            (cToShell input) `shouldBe` expected

        it "cToConst `hello_world` should equals `HELLO_WORLD`" $ do
            let expected = "HELLO_WORLD"
            let input = "hello_world"
            (cToConst input) `shouldBe` expected

        it "cToConst `fuck_you_trump` should equals `FUCK_YOU_TRUMP`" $ do
            let expected = "FUCK_YOU_TRUMP"
            let input = "fuck_you_trump"
            (cToConst input) `shouldBe` expected

