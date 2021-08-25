module Taskell.IO.MarkDown.Serializer.SerializeSpec
    ( spec
    ) where

import RIO
import qualified RIO.Seq as Seq
import qualified RIO.Text as T

import Test.Hspec

import Taskell.IO.MarkDown.Serializer.Serialize (serialize)
import Taskell.IO.MarkDown.Types

import qualified Taskell.Data.Types.Taskell as Taskell

import TestData (testData)

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
spec =
    parallel $ do
        describe "title" $ do
            it "serializes the title" $ do readLine 1 testData `shouldBe` Just "# Test"
            it "serializes the title" $ do
                readLine 1 (testData & Taskell.title .~ "Fish") `shouldBe` Just "# Fish"
        describe "description" $ do
            it "new line before" $ do readLine 2 testData `shouldBe` Just ""
            it "adds description" $ do readLine 3 testData `shouldBe` Just "Some test data"
        describe "contributors" $ do
            it "new line before" $ do readLine 4 testData `shouldBe` Just ""
            it "title" $ do readLine 5 testData `shouldBe` Just "## Contributors"
            it "new line before contributors" $ do readLine 6 testData `shouldBe` Just ""
            it "first contributor" $ do
                readLine 7 testData `shouldBe` Just "- **@Bob**: Bob (bob@bob.com)"
            it "second contributor" $ do
                readLine 8 testData `shouldBe` Just "- **@Jenny**: Jenny (jenny@jenny.com)"
            it "third contributor" $ do
                readLine 9 testData `shouldBe` Just "- **@Jim**: Jim (jim@jim.com)"
        describe "horizontal rule" $ do
            it "should have break before" $ do readLine 10 testData `shouldBe` Just ""
            it "horizontal rule" $ do readLine 11 testData `shouldBe` Just "---"
        describe "tasks" $ do
            describe "first list" $ do
                it "should have break before" $ do readLine 12 testData `shouldBe` Just ""
                it "title" $ do readLine 13 testData `shouldBe` Just "## First List"
                describe "first task" $ do
                    it "should have break before" $ do readLine 14 testData `shouldBe` Just ""
                    it "title" $ do readLine 15 testData `shouldBe` Just "### First Task"
                    it "line break" $ do readLine 16 testData `shouldBe` Just ""
                    it "description" $ do readLine 17 testData `shouldBe` Just "Do first thing"
                    it "blank line" $ do readLine 18 testData `shouldBe` Just ""
                    it "tags" $ do readLine 19 testData `shouldBe` Just "`#fish`, `#cow`"
                    it "blank line" $ do readLine 20 testData `shouldBe` Just ""
                    it "related" $ do
                        readLine 21 testData `shouldBe`
                            Just
                                "**Related**: [Second List / Second Task](#second-task), [First List / Third Task](#third-task), [First List / Fifth Task](#fifth-task)"
                    it "blank line" $ do readLine 22 testData `shouldBe` Just ""
                    it "contributors" $ do
                        readLine 23 testData `shouldBe` Just "**Contributors**: *@Bob*, *@Jenny*"
                    describe "sub-tasks" $ do
                        it "should have break before" $ do readLine 24 testData `shouldBe` Just ""
                        describe "first sub-task" $ do
                            it "title" $ do readLine 25 testData `shouldBe` Just "- [ ] Sub Task"
                            it "blank line" $ do readLine 26 testData `shouldBe` Just ""
                            it "description" $ do
                                readLine 27 testData `shouldBe` Just "    Sub task"
                            it "blank line" $ do readLine 28 testData `shouldBe` Just ""
                            describe "sub-sub-tasks" $ do
                                it "title" $ do
                                    readLine 29 testData `shouldBe` Just "    - [x] Sub Sub Task"
                                it "blank line" $ do readLine 30 testData `shouldBe` Just ""
                                it "tags" $ do
                                    readLine 31 testData `shouldBe` Just "        `#spatula`"
                                it "blank line" $ do readLine 32 testData `shouldBe` Just ""
                                describe "sub-sub-sub-tasks" $ do
                                    it "title" $ do
                                        readLine 33 testData `shouldBe`
                                            Just "        - [ ] Sub Sub Sub Task"
                                    it "blank line" $ do readLine 34 testData `shouldBe` Just ""
                                    it "description" $ do
                                        readLine 35 testData `shouldBe`
                                            Just "            Sub sub sub task"
                describe "next task" $ do
                    it "should have break before" $ do readLine 36 testData `shouldBe` Just ""
                    it "title" $ do readLine 37 testData `shouldBe` Just "### Third Task"
            describe "second list" $ do
                it "should have break before" $ do readLine 38 testData `shouldBe` Just ""
                it "title" $ do readLine 57 testData `shouldBe` Just "## Second List"
