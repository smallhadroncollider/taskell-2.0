module Taskell.Data.Taskell.TaskellSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.Data.TestData

import Taskell.Data.Types.List as L (ListID(..))

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
                         [ListID 2, ListID 1]
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
                         [ListID 2, ListID 1]
                         allTasks)
