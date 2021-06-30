module Taskell.UI.Text.ParserSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.UI.Text.Parser

-- tests
spec :: Spec
spec = do
    describe "parse" $ do
        it "empty string" $ parse "" `shouldBe` Right []
        it "single character" $ parse "a" `shouldBe` Right [Word "a"]
        it "single word" $ parse "hello" `shouldBe` Right [Word "hello"]
        it "multiple words" $
            parse "hello world" `shouldBe` Right [Word "hello", Whitespace " ", Word "world"]
        it "new line" $
            parse "hello\nworld" `shouldBe` Right [Word "hello", LineBreak, Word "world"]
        it "new lines" $
            parse "hello\n\nworld\n" `shouldBe`
            Right [Word "hello", LineBreak, LineBreak, Word "world", LineBreak]
        it "new line" $ parse "hello \n" `shouldBe` Right [Word "hello", Whitespace " ", LineBreak]
        it "new line - after space" $
            parse "hello \nworld" `shouldBe`
            Right [Word "hello", Whitespace " ", LineBreak, Word "world"]
        it "new lines - after spaces" $
            parse "hello    \n\nworld" `shouldBe`
            Right [Word "hello", Whitespace "    ", LineBreak, LineBreak, Word "world"]
        it "non-alphabetic characters" $ parse "hello_12" `shouldBe` Right [Word "hello_12"]
        it "non-ASCII characters" $
            parse "汉堡包漢堡 包汉堡漢堡" `shouldBe` Right [Word "汉堡包漢堡", Whitespace " ", Word "包汉堡漢堡"]
