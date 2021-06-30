module Taskell.UI.Text.SplitSpec
    ( spec
    ) where

import RIO
import qualified RIO.Seq as Seq

import Test.Hspec

import Taskell.UI.Text.Parser
import Taskell.UI.Text.Split

-- tests
spec :: Spec
spec = do
    describe "split" $ do
        describe "single line" $ do
            it "empty string" $ split 10 "" `shouldBe` Right (Seq.fromList [])
            it "single word" $
                split 20 "hello" `shouldBe` Right (Seq.fromList [([Word "hello"], 5)])
            it "two words" $
                split 11 "hello world" `shouldBe`
                Right (Seq.fromList [([Word "hello", Whitespace " ", Word "world"], 11)])
        describe "multiple lines" $ do
            it "two words" $
                split 5 "hello world" `shouldBe`
                Right (Seq.fromList [([Word "hello", Whitespace " "], 6), ([Word "world"], 5)])
            it "multiple words" $
                split 11 "hello world today" `shouldBe`
                Right
                    (Seq.fromList
                         [ ([Word "hello", Whitespace " ", Word "world", Whitespace " "], 12)
                         , ([Word "today"], 5)
                         ])
        describe "line breaks" $ do
            it "newline" $
                split 11 "hello\nworld today fish" `shouldBe`
                Right
                    (Seq.fromList
                         [ ([Word "hello"], 5)
                         , ( [LineBreak, Word "world", Whitespace " ", Word "today", Whitespace " "]
                           , 12)
                         , ([Word "fish"], 4)
                         ])
            it "multiple newline" $
                split 11 "hello\n\nworld today fish" `shouldBe`
                Right
                    (Seq.fromList
                         [ ([Word "hello"], 5)
                         , ([LineBreak], 0)
                         , ( [LineBreak, Word "world", Whitespace " ", Word "today", Whitespace " "]
                           , 12)
                         , ([Word "fish"], 4)
                         ])
            it "newline" $
                split 20 "hello \n" `shouldBe`
                Right (Seq.fromList [([Word "hello", Whitespace " "], 6), ([LineBreak], 0)])
            it "newline" $
                split 20 "hello \n\nh" `shouldBe`
                Right
                    (Seq.fromList
                         [ ([Word "hello", Whitespace " "], 6)
                         , ([LineBreak], 0)
                         , ([LineBreak, Word "h"], 1)
                         ])
    describe "withWidth" $ do
        describe "single line" $ do
            it "empty string" $ withWidth 10 "" `shouldBe` Right []
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
