module Taskell.IO.MarkDown.Convert.ToSerializedSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.IO.MarkDown.Convert.ToSerialized (convert)
import Taskell.IO.MarkDown.Types

import TmpData

-- tests
spec :: Spec
spec = do
    describe "title" $ do
        it "gets the title" $ do convert tmpData ^. taskellTitle `shouldBe` "Test"
        it "gets the description" $ do
            convert tmpData ^. taskellDescription `shouldBe` "Some test data"
        it "sorts the contributors alphabetically by name" $ do
            convert tmpData ^. taskellContributors `shouldBe`
                [ SerializedContributor "Bob" "Bob" "bob@bob.com"
                , SerializedContributor "Jenny" "Jenny" "jenny@jenny.com"
                , SerializedContributor "Jim" "Jim" "jim@jim.com"
                ]
