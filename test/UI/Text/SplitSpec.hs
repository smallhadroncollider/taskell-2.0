module UI.Text.SplitSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import UI.Text.Split (withWidth)

-- tests
spec :: Spec
spec = do
    describe "withWidth" $ do
        describe "single line" $ do
            it "empty string" $ withWidth 10 "" `shouldBe` Right [""]
            it "single word" $ withWidth 20 "hello" `shouldBe` Right ["hello"]
            it "two words" $ withWidth 11 "hello world" `shouldBe` Right ["hello world"]
        describe "multiple lines" $ do
            it "two words" $ withWidth 5 "hello world" `shouldBe` Right ["hello ", "world"]
            it "multiple words" $
                withWidth 11 "hello world today" `shouldBe` Right ["hello world ", "today"]
        describe "line breaks" $ do
            it "newline" $
                withWidth 11 "hello\nworld today fish" `shouldBe`
                Right ["hello", "world today ", "fish"]
            it "multiple newline" $
                withWidth 11 "hello\n\nworld today fish" `shouldBe`
                Right ["hello", "", "world today ", "fish"]
        describe "non-ASCII characters" $ do
            it "double-width characters" $ withWidth 5 "汉堡 包汉" `shouldBe` Right ["汉堡 ", "包汉"]
        describe "long-words" $ do
            it "should break" $ withWidth 5 "helloworld" `shouldBe` Right ["hello", "world"]
            it "should break" $
                withWidth 5 "helloworldfishycakes" `shouldBe`
                Right ["hello", "world", "fishy", "cakes"]
            it "should break" $
                withWidth 10 "today helloworldfish spoon" `shouldBe`
                Right ["today ", "helloworld", "fish spoon"]
