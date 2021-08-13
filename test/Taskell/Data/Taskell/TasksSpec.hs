module Taskell.Data.Taskell.TasksSpec
    ( spec
    ) where

import RIO
import qualified RIO.HashMap as HM (fromList, insert)
import qualified RIO.Seq as Seq

import Test.Hspec

import TmpData

import Taskell.Data.Taskell
import Taskell.Data.Types.List as L
import Taskell.Data.Types.NextID (setTaskID)
import Taskell.Data.Types.Task as T
import qualified Taskell.Data.Types.Taskell as Taskell

import qualified Taskell.Error as Error

testData :: Taskell
testData = tmpData

-- tests
spec :: Spec
spec =
    parallel $ do
        describe "Taskell Tasks" $ do
            describe "gets tasks for tasks" $ do
                it "task 1" $
                    tasksForTask (TaskID 1) testData `shouldBe` Right (Seq.fromList [task6])
                it "task 6" $
                    tasksForTask (TaskID 6) testData `shouldBe` Right (Seq.fromList [task7])
                it "task 7" $
                    tasksForTask (TaskID 7) testData `shouldBe` Right (Seq.fromList [task8])
                it "task 8" $ tasksForTask (TaskID 8) testData `shouldBe` Right []
            describe "adds tasks" $ do
                it "add task" $
                    snd <$>
                    addTaskToList "Blah" (ListID 1) testData `shouldBe`
                    Right
                        (Taskell
                             "Test"
                             "Some test data"
                             allContributors
                             (HM.fromList
                                  [ ( ListID 1
                                    , list1 & L.tasks .~ (TaskID <$> Seq.fromList [1, 3, 5, 9]))
                                  , (ListID 2, list2)
                                  ])
                             allListsOrder
                             (HM.insert
                                  (TaskID 9)
                                  (Task "Blah" (ParentList (ListID 1)) "" False [] [] [] [])
                                  allTasks)
                             allTags
                             (setTaskID 9 allIDs))
            describe "renames task" $ do
                it "rename task" $
                    renameTask "Blah" (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~ HM.insert (TaskID 1) (task1 & T.title .~ "Blah"))
            describe "moves task" $ do
                it "moves up" $
                    moveTaskUp (TaskID 3) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.lists %~
                         HM.insert
                             (ListID 1)
                             (list1 & L.tasks .~ (TaskID <$> Seq.fromList [3, 1, 5])))
                it "moves down" $
                    moveTaskDown (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.lists %~
                         HM.insert
                             (ListID 1)
                             (list1 & L.tasks .~ (TaskID <$> Seq.fromList [3, 1, 5])))
                it "moves left (to bottom)" $
                    moveTaskLeft (TaskID 2) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 2) (task2 & T.parent .~ ParentList (ListID 1)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [1, 3, 5, 2]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [4]))
                             ])
                it "moves left (to bottom) - no change" $
                    moveTaskLeft (TaskID 1) testData `shouldBe` Right testData
                it "moves right (to bottom)" $
                    moveTaskRight (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 1) (task1 & T.parent .~ ParentList (ListID 2)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [3, 5]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [2, 4, 1]))
                             ])
                it "moves right (to bottom) - no change" $
                    moveTaskRight (TaskID 2) testData `shouldBe` Right testData
                it "moves left (to top)" $
                    moveTaskLeftTop (TaskID 2) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 2) (task2 & T.parent .~ ParentList (ListID 1)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [2, 1, 3, 5]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [4]))
                             ])
                it "moves left (to top) - no change" $
                    moveTaskLeftTop (TaskID 1) testData `shouldBe` Right testData
                it "moves right (to top)" $
                    moveTaskRightTop (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 1) (task1 & T.parent .~ ParentList (ListID 2)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [3, 5]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [1, 2, 4]))
                             ])
                it "moves right (to top) - no change" $
                    moveTaskRightTop (TaskID 2) testData `shouldBe` Right testData
            describe "changes task description" $ do
                it "change description" $
                    changeTaskDescription "Blah" (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~ HM.insert (TaskID 1) (task1 & T.description .~ "Blah"))
            describe "removes tasks" $ do
                it "task 1" $
                    removeTasks (TaskID 1) testData `shouldBe`
                    Right
                        (Taskell
                             "Test"
                             "Some test data"
                             allContributors
                             (HM.fromList
                                  [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [3, 5]))
                                  , (ListID 2, list2)
                                  ])
                             allListsOrder
                             (HM.fromList
                                  [ ( TaskID 2
                                    , task2 & T.tasks .~ [] &
                                      T.related .~ (TaskID <$> Seq.fromList [3]))
                                  , (TaskID 3, task3 & T.related .~ (TaskID <$> Seq.fromList [2]))
                                  , (TaskID 4, task4)
                                  , (TaskID 5, task5 & T.related .~ [])
                                  ])
                             allTags
                             allIDs)
                it "no task" $
                    removeTasks (TaskID 99) testData `shouldBe`
                    Error.e "Unknown reference: TaskID 99"
