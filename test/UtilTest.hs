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

        it "cToPascal `fuck_you_trump` should equals `fuckYouTrump`" $ do
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

