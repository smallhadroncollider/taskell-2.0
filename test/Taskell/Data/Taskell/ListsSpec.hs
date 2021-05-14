module Taskell.Data.Taskell.ListsSpec
    ( spec
    ) where

import RIO
import qualified RIO.HashMap as HM (fromList)

import Test.Hspec

import Taskell.Data.TestData

import Taskell.Data.Types.List as L (List(..), ListID(..), title)
import Taskell.Data.Types.Taskell (listsOrder)

import Taskell.Data.Taskell
    ( Taskell(..)
    , addList
    , e
    , getLists
    , moveListLeft
    , moveListRight
    , removeList
    , renameList
    , tasksForList
    )

-- tests
spec :: Spec
spec = do
    describe "Taskell Lists" $ do
        it "gets lists" $ getLists testData `shouldBe` Right [list2, list1]
        describe "adds lists" $ do
            it "list 3" $
                addList "Third List" (ListID 3) testData `shouldBe`
                Right
                    (Taskell
                         "Test"
                         "Some test data"
                         allContributors
                         (HM.fromList
                              [ (ListID 1, list1)
                              , (ListID 2, list2)
                              , (ListID 3, List "Third List" [])
                              ])
                         [ListID 2, ListID 1, ListID 3]
                         allTasks)
        describe "removes lists" $ do
            it "list 1" $
                removeList (ListID 1) testData `shouldBe`
                Right
                    (Taskell
                         "Test"
                         "Some test data"
                         allContributors
                         (HM.fromList [(ListID 2, list2)])
                         [ListID 2]
                         allTasks)
        describe "reorders lists" $ do
            it "list 1 left" $
                moveListLeft (ListID 1) testData `shouldBe`
                Right (testData & listsOrder .~ [ListID 1, ListID 2])
            it "list 2 left" $
                moveListLeft (ListID 2) testData `shouldBe`
                Right (testData & listsOrder .~ [ListID 2, ListID 1])
            it "list 1 right" $
                moveListRight (ListID 1) testData `shouldBe`
                Right (testData & listsOrder .~ [ListID 2, ListID 1])
            it "list 2 right" $
                moveListRight (ListID 2) testData `shouldBe`
                Right (testData & listsOrder .~ [ListID 1, ListID 2])
        describe "renames lists" $ do
            it "list 1" $
                renameList "List 1 Changed" (ListID 1) testData `shouldBe`
                Right
                    (Taskell
                         "Test"
                         "Some test data"
                         allContributors
                         (HM.fromList
                              [(ListID 1, list1 & L.title .~ "List 1 Changed"), (ListID 2, list2)])
                         [ListID 2, ListID 1]
                         allTasks)
        describe "gets tasks for lists" $ do
            it "list 1" $ tasksForList (ListID 1) testData `shouldBe` Right [task1, task5, task3]
            it "list 2" $ tasksForList (ListID 2) testData `shouldBe` Right [task2, task4]
            it "no list" $
                tasksForList (ListID 3) testData `shouldBe` e "Unknown reference: ListID 3"
