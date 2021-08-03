module Taskell.IO.MarkDown.Serializer.SerializeSpec
    ( spec
    ) where

import RIO
import qualified RIO.Seq as Seq
import qualified RIO.Text as T

import Test.Hspec

import Taskell.IO.MarkDown.Serializer.Serialize (serialize)
import Taskell.IO.MarkDown.Types (defaultDictionary)

import qualified Taskell.Data.Types.Taskell as Taskell

import TmpData

readLine :: Int -> Taskell.Taskell -> Maybe Text
readLine number tsk =
    case serialize defaultDictionary tsk of
        Left _ -> Nothing
        Right txt -> do
            let serialized = utf8BuilderToText txt
            let lns = Seq.fromList (T.split (== '\n') serialized)
            (Seq.!?) lns (number - 1)

-- tests
spec :: Spec
spec = do
    describe "title" $ do
        it "serializes the title" $ do readLine 1 tmpData `shouldBe` Just "# Test"
        it "serializes the title" $ do
            readLine 1 (tmpData & Taskell.title .~ "Fish") `shouldBe` Just "# Fish"
    describe "description" $ do
        it "new line before" $ do readLine 2 tmpData `shouldBe` Just ""
        it "adds description" $ do readLine 3 tmpData `shouldBe` Just "Some test data"
    describe "contributors" $ do
        it "new line before" $ do readLine 4 tmpData `shouldBe` Just ""
        it "title" $ do readLine 5 tmpData `shouldBe` Just "## Contributors"
        it "new line before contributors" $ do readLine 6 tmpData `shouldBe` Just ""
        it "first contributor" $ do readLine 7 tmpData `shouldBe` Just "- @Bob: Bob (bob@bob.com)"
        it "second contributor" $ do
            readLine 8 tmpData `shouldBe` Just "- @Jenny: Jenny (jenny@jenny.com)"
        it "third contributor" $ do readLine 9 tmpData `shouldBe` Just "- @Jim: Jim (jim@jim.com)"
    describe "horizontal rule" $ do
        it "should have break before" $ do readLine 10 tmpData `shouldBe` Just ""
        it "horizontal rule" $ do readLine 11 tmpData `shouldBe` Just "---"
    describe "tasks" $ do
        describe "first list" $ do
            it "should have break before" $ do readLine 12 tmpData `shouldBe` Just ""
            it "title" $ do readLine 13 tmpData `shouldBe` Just "## First List"
            describe "first task" $ do
                it "should have break before" $ do readLine 14 tmpData `shouldBe` Just ""
                it "title" $ do readLine 15 tmpData `shouldBe` Just "### First Task"
                it "line break" $ do readLine 16 tmpData `shouldBe` Just ""
                it "description" $ do readLine 17 tmpData `shouldBe` Just "Do first thing"
                describe "sub-tasks" $ do
                    it "should have break before" $ do readLine 18 tmpData `shouldBe` Just ""
                    describe "first sub-task" $ do
                        it "title" $ do readLine 19 tmpData `shouldBe` Just "- [ ] Sub Task"
                        it "blank line" $ do readLine 20 tmpData `shouldBe` Just ""
                        it "description" $ do readLine 21 tmpData `shouldBe` Just "    Sub task"
                        it "blank line" $ do readLine 22 tmpData `shouldBe` Just ""
                        describe "sub-sub-tasks" $ do
                            it "title" $ do
                                readLine 23 tmpData `shouldBe` Just "    - [x] Sub Sub Task"
                            it "blank line" $ do readLine 24 tmpData `shouldBe` Just ""
                            it "description" $ do
                                readLine 25 tmpData `shouldBe` Just "        Sub sub task"
                            it "blank line" $ do readLine 26 tmpData `shouldBe` Just ""
                            describe "sub-sub-sub-tasks" $ do
                                it "title" $ do
                                    readLine 27 tmpData `shouldBe`
                                        Just "        - [ ] Sub Sub Sub Task"
                                it "blank line" $ do readLine 28 tmpData `shouldBe` Just ""
                                it "description" $ do
                                    readLine 29 tmpData `shouldBe`
                                        Just "            Sub sub sub task"
                                it "blank line" $ do readLine 30 tmpData `shouldBe` Just ""
                it "tags" $ do readLine 31 tmpData `shouldBe` Just "`#fish`, `#cow`"
                it "blank line" $ do readLine 32 tmpData `shouldBe` Just ""
                it "related" $ do
                    readLine 33 tmpData `shouldBe`
                        Just "**Related**: [First List / Fifth Task](#fifth-task)"
                it "blank line" $ do readLine 34 tmpData `shouldBe` Just ""
                it "contributors" $ do
                    readLine 35 tmpData `shouldBe` Just "**Contributors**: @Bob, @Jim"
            describe "next task" $ do
                it "should have break before" $ do readLine 36 tmpData `shouldBe` Just ""
                it "title" $ do readLine 37 tmpData `shouldBe` Just "### Third Task"
        describe "second list" $ do
            it "should have break before" $ do readLine 56 tmpData `shouldBe` Just ""
            it "title" $ do readLine 57 tmpData `shouldBe` Just "## Second List"
