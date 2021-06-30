module Taskell.UI.Text.EditorSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.Error (e)
import Taskell.UI.Text.Editor

-- tests
spec :: Spec
spec = do
    describe "begin" $ do
        it "goes to first character" $
            ((^. cursor) <$> (begin =<< create 20 "Hello")) `shouldBe` Right (Cursor 0 0)
    describe "end" $ do
        it "goes to last character" $
            ((^. cursor) <$> create 20 "Hello") `shouldBe` Right (Cursor 5 0)
        it "goes to last character" $
            ((^. cursor) <$> create 20 "Hello how are you today? My name is Em.") `shouldBe`
            Right (Cursor 3 2)
        it "goes to last character of last line" $
            ((^. cursor) <$> create 20 "Hello\nMum") `shouldBe` Right (Cursor 3 1)
    describe "right" $ do
        it "goes right one character" $
            ((^. cursor) <$> (right =<< begin =<< create 20 "Hello")) `shouldBe` Right (Cursor 1 0)
        it "doesn't go right one character" $
            ((^. cursor) <$> (right =<< create 20 "Hello")) `shouldBe` Right (Cursor 5 0)
    describe "left" $ do
        it "goes left one character" $
            ((^. cursor) <$> (left =<< create 20 "Hello")) `shouldBe` Right (Cursor 4 0)
        it "doesn't go left one character" $
            ((^. cursor) <$> (left =<< begin =<< create 20 "Hello")) `shouldBe` Right (Cursor 0 0)
    describe "up" $ do
        it "goes up one row" $
            ((^. cursor) <$> (up =<< create 5 "Hello Today Fish")) `shouldBe` Right (Cursor 4 1)
        it "doesn't go up one row" $
            ((^. cursor) <$> (up =<< begin =<< create 20 "Hello")) `shouldBe` Right (Cursor 0 0)
        it "doesn't go outside row length" $
            ((^. cursor) <$> (up =<< create 5 "Hello Cow Spoon")) `shouldBe` Right (Cursor 4 1)
    describe "down" $ do
        it "goes down one row" $
            ((^. cursor) <$> (down =<< begin =<< create 5 "Hello Fish")) `shouldBe`
            Right (Cursor 0 1)
        it "doesn't go down one row" $
            ((^. cursor) <$> (down =<< create 20 "Hello")) `shouldBe` Right (Cursor 5 0)
        it "doesn't go outside row length" $
            ((^. cursor) <$> (down =<< endOfLine =<< begin =<< create 5 "Hello Cow Spoon")) `shouldBe`
            Right (Cursor 4 1)
    describe "top" $ do
        it "goes to the top row" $
            ((^. cursor) <$> (top =<< create 5 "Hello Fish")) `shouldBe` Right (Cursor 4 0)
    describe "bottom" $ do
        it "goes to the bottom row" $
            ((^. cursor) <$> (bottom =<< begin =<< create 5 "Hello Fish")) `shouldBe`
            Right (Cursor 0 1)
    describe "endOfLine" $ do
        it "goes to end of line" $
            ((^. cursor) <$> (endOfLine =<< begin =<< create 5 "Hello Fish")) `shouldBe`
            Right (Cursor 6 0)
        it "goes to end of line" $
            ((^. cursor) <$> (endOfLine =<< begin =<< create 5 "Hello    Fish")) `shouldBe`
            Right (Cursor 9 0)
        it "goes to end of line" $
            ((^. cursor) <$> (endOfLine =<< create 5 "Hello Fish")) `shouldBe` Right (Cursor 4 1)
    describe "getRelativePosition" $ do
        it "works out position" $ do
            (getRelativePosition =<< create 20 "Hello Mum") `shouldBe` Right 9
        it "works out position" $ do
            (getRelativePosition =<< create 5 "Hello Mum") `shouldBe` Right 9
        it "works out position" $ do
            (getRelativePosition =<< (begin =<< create 5 "Hello Mum")) `shouldBe` Right 0
        it "works out position" $ do
            (getRelativePosition =<< create 5 "Hello\nMum") `shouldBe` Right 9
        it "works out position" $ do
            (getRelativePosition =<< create 5 "Hello\n\n\nMum") `shouldBe` Right 11
        it "works out position" $ do
            (getRelativePosition =<< create 20 "hello \n") `shouldBe` Right 7
        it "works out position" $ do
            (getRelativePosition =<< create 20 "hello \n\n") `shouldBe` Right 8
    describe "setCursorFromRelativePosition" $ do
        it "works out position" $ do
            ((^. cursor) <$> (setCursorFromRelativePosition 0 =<< create 20 "Hello Mum")) `shouldBe`
                Right (Cursor 0 0)
        it "works out position" $ do
            ((^. cursor) <$> (setCursorFromRelativePosition 2 =<< create 5 "Hello Mum")) `shouldBe`
                Right (Cursor 2 0)
        it "works out position" $ do
            ((^. cursor) <$> (setCursorFromRelativePosition 5 =<< create 20 "Hello")) `shouldBe`
                Right (Cursor 5 0)
        it "works out position - multi-line" $ do
            ((^. cursor) <$> (setCursorFromRelativePosition 7 =<< create 5 "Hello Mum")) `shouldBe`
                Right (Cursor 1 1)
        it "works out position - multi-line" $ do
            ((^. cursor) <$> (setCursorFromRelativePosition 9 =<< create 5 "Hello Mum")) `shouldBe`
                Right (Cursor 3 1)
        it "works out position - spaces" $ do
            ((^. cursor) <$> (setCursorFromRelativePosition 6 =<< create 20 "Hello ")) `shouldBe`
                Right (Cursor 6 0)
        describe "newlines" $ do
            it "works out position - new lines" $ do
                ((^. cursor) <$> (setCursorFromRelativePosition 7 =<< create 20 "Hello\nMum")) `shouldBe`
                    Right (Cursor 1 1)
            it "works out position - new lines" $ do
                ((^. cursor) <$> (setCursorFromRelativePosition 5 =<< create 20 "hello\n")) `shouldBe`
                    Right (Cursor 5 0)
            it "works out position - new lines" $ do
                ((^. cursor) <$> (setCursorFromRelativePosition 6 =<< create 20 "hello\n")) `shouldBe`
                    Right (Cursor 0 1)
            it "works out position - new lines" $ do
                ((^. cursor) <$> (setCursorFromRelativePosition 7 =<< create 20 "hello\n\n")) `shouldBe`
                    Right (Cursor 0 2)
            it "works out position - new lines" $ do
                ((^. cursor) <$> (setCursorFromRelativePosition 8 =<< create 20 "hello\n\nh")) `shouldBe`
                    Right (Cursor 1 2)
            it "works out position - new lines" $ do
                ((^. cursor) <$> (setCursorFromRelativePosition 9 =<< create 20 "Hello\n\n\nMum")) `shouldBe`
                    Right (Cursor 1 3)
            describe "spaces on end" $ do
                it "works out position - new lines" $ do
                    ((^. cursor) <$> (setCursorFromRelativePosition 5 =<< create 20 "hello \n")) `shouldBe`
                        Right (Cursor 5 0)
                it "works out position - new lines" $ do
                    ((^. cursor) <$> (setCursorFromRelativePosition 7 =<< create 20 "hello \n")) `shouldBe`
                        Right (Cursor 0 1)
                it "works out position - new lines" $ do
                    ((^. cursor) <$> (setCursorFromRelativePosition 8 =<< create 20 "hello \n\n")) `shouldBe`
                        Right (Cursor 0 2)
                it "works out position - new lines" $ do
                    ((^. cursor) <$>
                     (setCursorFromRelativePosition 8 =<< insert "\n" =<< create 20 "hello \n")) `shouldBe`
                        Right (Cursor 0 2)
                it "works out position - new lines" $ do
                    ((^. cursor) <$> (setCursorFromRelativePosition 9 =<< create 20 "hello \n\nh")) `shouldBe`
                        Right (Cursor 1 2)
                it "works out position - new lines" $ do
                    ((^. cursor) <$>
                     (setCursorFromRelativePosition 9 =<< insert "h" =<< create 20 "hello \n\n")) `shouldBe`
                        Right (Cursor 1 2)
                it "works out position - new lines" $ do
                    ((^. cursor) <$>
                     (setCursorFromRelativePosition 10 =<< create 20 "hello \n\n\nh")) `shouldBe`
                        Right (Cursor 1 3)
            describe "errors" $ do
                it "error" $ do
                    ((^. cursor) <$> (setCursorFromRelativePosition (-1) =<< create 20 "hello\n\nh")) `shouldBe`
                        e "Cursor: no rows are shorter than position"
    describe "insert" $ do
        it "adds on end" $ dump <$> (insert "x" =<< create 20 "Hello") `shouldBe` Right "Hellox"
        it "adds on end - longer" $
            dump <$>
            (insert "x" =<< create 20 "Hello how are you today? My name is Em.") `shouldBe`
            Right "Hello how are you today? My name is Em.x"
        it "keeps new lines" $
            dump <$> (insert "x" =<< create 20 "Hello\nMum") `shouldBe` Right "Hello\nMumx"
        describe "cursor" $ do
            it "updates position" $
                ((^. cursor) <$> (insert "x" =<< create 20 "Hello")) `shouldBe` Right (Cursor 6 0)
            it "updates position over lines" $
                ((^. cursor) <$> (insert "x" =<< endOfLine =<< top =<< create 5 "Hello Mum")) `shouldBe`
                Right (Cursor 1 1)
            it "updates position" $
                ((^. cursor) <$> (insert "🙀" =<< create 20 "")) `shouldBe` Right (Cursor 1 0)
            it "updates position" $
                ((^. cursor) <$> (insert "today is" =<< create 5 "Hello")) `shouldBe`
                Right (Cursor 2 2)
    describe "backspace" $ do
        it "removes from end" $ dump <$> (backspace =<< create 20 "Hello") `shouldBe` Right "Hell"
        it "removes from end - longer" $
            dump <$>
            (backspace =<< create 20 "Hello how are you today? My name is Em.") `shouldBe`
            Right "Hello how are you today? My name is Em"
        it "keeps new lines" $
            dump <$>
            (backspace =<< endOfLine =<< begin =<< create 20 "Hello\nMum") `shouldBe`
            Right "Hell\nMum"
        describe "cursor" $ do
            it "updates position" $
                ((^. cursor) <$> (backspace =<< create 20 "Hello")) `shouldBe` Right (Cursor 4 0)
            it "updates position" $
                ((^. cursor) <$> (backspace =<< create 20 "Hello\nMum")) `shouldBe`
                Right (Cursor 2 1)
    describe "dump" $ do
        describe "single line" $ do
            it "empty string" $ dump <$> create 10 "" `shouldBe` Right ""
            it "single word" $ dump <$> create 20 "hello" `shouldBe` Right "hello"
            it "two words" $ dump <$> create 11 "hello world" `shouldBe` Right "hello world"
        describe "multiple lines" $ do
            it "two words" $ dump <$> create 5 "hello world" `shouldBe` Right "hello world"
            it "multiple words" $
                dump <$> create 11 "hello world today" `shouldBe` Right "hello world today"
        describe "line breaks" $ do
            it "newline" $
                dump <$>
                create 11 "hello\nworld today fish" `shouldBe` Right "hello\nworld today fish"
            it "multiple newline" $
                dump <$>
                create 11 "hello\n\nworld today fish" `shouldBe` Right "hello\n\nworld today fish"
        describe "non-ASCII characters" $ do
            it "double-width characters" $ dump <$> create 5 "汉堡 包汉" `shouldBe` Right "汉堡 包汉"
        describe "long-words" $ do
            it "should break" $ dump <$> create 5 "helloworld" `shouldBe` Right "helloworld"
            it "should break" $
                dump <$> create 5 "helloworldfishycakes" `shouldBe` Right "helloworldfishycakes"
            it "should break" $
                dump <$>
                create 10 "today helloworldfish spoon" `shouldBe` Right "today helloworldfish spoon"
    describe "parses multiple line breaks" $ do
        it "empty string" $
            (^. cursor) <$> (end =<< create 10 "Hello\n\n\n\nToday") `shouldBe` Right (Cursor 5 4)
    describe "complex cursor" $ do
        it "correct position on empty lines" $
            ((^. cursor) <$> (up =<< create 20 "Hello\n\nFish")) `shouldBe` Right (Cursor 0 1)
        it "correct position on empty lines" $
            ((^. cursor) <$> (down =<< begin =<< create 20 "Hello\n\nFish")) `shouldBe`
            Right (Cursor 0 1)
        it "correct position" $
            ((^. cursor) <$> (down =<< endOfLine =<< begin =<< create 20 "A\nB")) `shouldBe`
            Right (Cursor 1 1)
