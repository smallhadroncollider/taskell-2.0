module Taskell.Data.Taskell.TaskellSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import TmpData

import Taskell.Data.Taskell (Taskell(..), changeDescription, rename)

testData :: Taskell
testData = tmpData

-- tests
spec :: Spec
spec = do
    describe "Taskell" $ do
        describe "renames" $ do
            it "changes title" $
                rename "Test Changed" testData `shouldBe`
                Right
                    (Taskell
                         "Test Changed"
                         "Some test data"
                         allContributors
                         allLists
                         allListsOrder
                         allTasks
                         allTags)
        describe "changes description" $ do
            it "changes description" $
                changeDescription "Some test data changed" testData `shouldBe`
                Right
                    (Taskell
                         "Test"
                         "Some test data changed"
                         allContributors
                         allLists
                         allListsOrder
                         allTasks
                         allTags)
