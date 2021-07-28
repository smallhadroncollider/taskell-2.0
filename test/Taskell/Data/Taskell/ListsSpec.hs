module Taskell.Data.Taskell.ListsSpec
    ( spec
    ) where

import RIO
import qualified RIO.HashMap as HM (fromList)
import qualified RIO.Seq as Seq

import Test.Hspec

import TmpData

import Taskell.Data.Types.List as L (List(..), ListID(..), title)
import Taskell.Data.Types.Taskell (listsOrder)

import Taskell.Data.Taskell
    ( Taskell(..)
    , addList
    , getLists
    , getListsWithIDs
    , moveListLeft
    , moveListRight
    , removeList
    , renameList
    , tasksForList
    )

import qualified Taskell.Error as Error

testData :: Taskell
testData = tmpData

-- tests
spec :: Spec
spec = do
    describe "Taskell Lists" $ do
        it "gets lists" $ getLists testData `shouldBe` Right (Seq.fromList [list1, list2])
        it "gets lists with IDs" $
            getListsWithIDs testData `shouldBe`
            Right (Seq.fromList [(ListID 1, list1), (ListID 2, list2)])
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
                         (Seq.fromList [ListID 1, ListID 2, ListID 3])
                         allTasks
                         allTags)
        describe "removes lists" $ do
            it "list 1" $
                removeList (ListID 1) testData `shouldBe`
                Right
                    (Taskell
                         "Test"
                         "Some test data"
                         allContributors
                         (HM.fromList [(ListID 2, list2)])
                         (Seq.fromList [ListID 2])
                         allTasks
                         allTags)
        describe "reorders lists" $ do
            it "list 2 left" $
                moveListLeft (ListID 2) testData `shouldBe`
                Right (testData & listsOrder .~ Seq.fromList [ListID 2, ListID 1])
            it "list 1 left" $ moveListLeft (ListID 1) testData `shouldBe` Right testData
            it "list 2 right" $ moveListRight (ListID 2) testData `shouldBe` Right testData
            it "list 1 right" $
                moveListRight (ListID 1) testData `shouldBe`
                Right (testData & listsOrder .~ Seq.fromList [ListID 2, ListID 1])
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
                         allListsOrder
                         allTasks
                         allTags)
        describe "gets tasks for lists" $ do
            it "list 1" $
                tasksForList (ListID 1) testData `shouldBe`
                Right (Seq.fromList [task1, task3, task5])
            it "list 2" $
                tasksForList (ListID 2) testData `shouldBe` Right (Seq.fromList [task2, task4])
            it "no list" $
                tasksForList (ListID 3) testData `shouldBe` Error.e "Unknown reference: ListID 3"
