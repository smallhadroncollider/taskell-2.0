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
readLine number tsk = (Seq.!?) lns (number - 1)
  where
    serialized = utf8BuilderToText (serialize defaultDictionary tsk)
    lns = Seq.fromList (T.split (== '\n') serialized)

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
