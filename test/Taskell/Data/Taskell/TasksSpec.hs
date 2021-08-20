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
                    tasksForTask (TaskID 1) testData `shouldBe` Right (Seq.fromList [subTask])
                it "task 6" $
                    tasksForTask (TaskID 2) testData `shouldBe` Right (Seq.fromList [subSubTask])
                it "task 7" $
                    tasksForTask (TaskID 3) testData `shouldBe` Right (Seq.fromList [subSubSubTask])
                it "task 8" $ tasksForTask (TaskID 4) testData `shouldBe` Right []
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
                                    , list1 & L.tasks .~ (TaskID <$> Seq.fromList [1, 5, 6, 9]))
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
                         Taskell.tasks %~ HM.insert (TaskID 1) (list1Task1 & T.title .~ "Blah"))
            describe "moves task" $ do
                it "moves up" $
                    moveTaskUp (TaskID 5) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.lists %~
                         HM.insert
                             (ListID 1)
                             (list1 & L.tasks .~ (TaskID <$> Seq.fromList [5, 1, 6])))
                it "moves down" $
                    moveTaskDown (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.lists %~
                         HM.insert
                             (ListID 1)
                             (list1 & L.tasks .~ (TaskID <$> Seq.fromList [5, 1, 6])))
                it "moves left (to bottom)" $
                    moveTaskLeft (TaskID 7) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 7) (list2Task1 & T.parent .~ ParentList (ListID 1)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [1, 5, 6, 7]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [8]))
                             ])
                it "moves left (to bottom) - no change" $
                    moveTaskLeft (TaskID 1) testData `shouldBe` Right testData
                it "moves right (to bottom)" $
                    moveTaskRight (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 1) (list1Task1 & T.parent .~ ParentList (ListID 2)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [5, 6]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [7, 8, 1]))
                             ])
                it "moves right (to bottom) - no change" $
                    moveTaskRight (TaskID 7) testData `shouldBe` Right testData
                it "moves left (to top)" $
                    moveTaskLeftTop (TaskID 7) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 7) (list2Task1 & T.parent .~ ParentList (ListID 1)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [7, 1, 5, 6]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [8]))
                             ])
                it "moves left (to top) - no change" $
                    moveTaskLeftTop (TaskID 1) testData `shouldBe` Right testData
                it "moves right (to top)" $
                    moveTaskRightTop (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~
                         HM.insert (TaskID 1) (list1Task1 & T.parent .~ ParentList (ListID 2)) &
                         Taskell.lists .~
                         HM.fromList
                             [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [5, 6]))
                             , (ListID 2, list2 & L.tasks .~ (TaskID <$> Seq.fromList [1, 7, 8]))
                             ])
                it "moves right (to top) - no change" $
                    moveTaskRightTop (TaskID 7) testData `shouldBe` Right testData
            describe "changes task description" $ do
                it "change description" $
                    changeTaskDescription "Blah" (TaskID 1) testData `shouldBe`
                    Right
                        (testData &
                         Taskell.tasks %~ HM.insert (TaskID 1) (list1Task1 & T.description .~ "Blah"))
            describe "removes tasks" $ do
                it "task 1" $
                    removeTasks (TaskID 1) testData `shouldBe`
                    Right
                        (Taskell
                             "Test"
                             "Some test data"
                             allContributors
                             (HM.fromList
                                  [ (ListID 1, list1 & L.tasks .~ (TaskID <$> Seq.fromList [5, 6]))
                                  , (ListID 2, list2)
                                  ])
                             allListsOrder
                             (HM.fromList
                                  [ ( TaskID 7
                                    , list2Task1 & T.tasks .~ [] &
                                      T.related .~ (TaskID <$> Seq.fromList [5]))
                                  , (TaskID 5, list1Task2 & T.related .~ (TaskID <$> Seq.fromList [7]))
                                  , (TaskID 8, list2Task2)
                                  , (TaskID 6, list1Task3 & T.related .~ [])
                                  ])
                             allTags
                             allIDs)
                it "no task" $
                    removeTasks (TaskID 99) testData `shouldBe`
                    Error.e "Unknown reference: TaskID 99"
