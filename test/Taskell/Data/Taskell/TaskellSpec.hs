module Taskell.Data.Taskell.TaskellSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.Data.TestData

import Taskell.Data.Taskell (Taskell(..), changeDescription, rename)

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
                         allTasks)
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
                         allTasks)
