module Taskell.Data.Taskell.TasksSpec (spec) where

import RIO
import qualified RIO.HashMap as HM (fromList, insert)

import Test.Hspec

import Taskell.Data.TestData

import Taskell.Data.Types.List as L (ListID (..), tasks)
import Taskell.Data.Types.Task as T (Task (..), TaskID (..), Parent (..), tasks, related, title, description)
import qualified Taskell.Data.Types.Taskell as Taskell
import Taskell.Data.Taskell (Taskell (..), tasksForTask, removeTasks, addTaskToList, renameTask, changeTaskDescription, moveTaskUp, moveTaskDown)


-- tests
spec :: Spec
spec = do
    describe "Taskell Tasks" $ do
        describe "gets tasks for tasks" $ do
            it "task 1" $ tasksForTask (TaskID 1) testData `shouldBe` [task6]
            it "task 6" $ tasksForTask (TaskID 6) testData `shouldBe` [task7]
            it "task 7" $ tasksForTask (TaskID 7) testData `shouldBe` [task8]
            it "task 8" $ tasksForTask (TaskID 8) testData `shouldBe` []

        describe "adds tasks" $ do
            it "add task" $ addTaskToList "Blah" (TaskID 10) (ListID 1) testData `shouldBe` Taskell
                  "Test"
                  "Some test data"
                  allContributors
                  (HM.fromList [
                      (ListID 1, list1 & L.tasks .~ (TaskID <$> [1, 5, 3, 10]))
                  ,   (ListID 2, list2)
                  ])
                  allListsOrder
                  (HM.insert (TaskID 10) (Task "Blah" (ParentList (ListID 1)) "" [] [] [] []) allTasks)

        describe "renames task" $ do
            it "rename task" $ renameTask "Blah" (TaskID 1) testData `shouldBe`
                (testData & Taskell.tasks %~ HM.insert (TaskID 1) (task1 & T.title .~ "Blah"))

        describe "moves task" $ do
            it "moves up" $ moveTaskUp (TaskID 3) testData `shouldBe`
                (testData & Taskell.lists %~
                    HM.insert (ListID 1) (list1 & L.tasks .~ (TaskID <$> [1, 3, 5]))
                )

            it "moves down" $ moveTaskDown (TaskID 1) testData `shouldBe`
                (testData & Taskell.lists %~
                    HM.insert (ListID 1) (list1 & L.tasks .~ (TaskID <$> [5, 1, 3]))
                )


        describe "changes task description" $ do
            it "change description" $ changeTaskDescription "Blah" (TaskID 1) testData `shouldBe`
                (testData & Taskell.tasks %~ HM.insert (TaskID 1) (task1 & T.description .~ "Blah"))

        describe "removes tasks" $ do
            it "task 1" $ removeTasks (TaskID 1) testData `shouldBe` Taskell
                  "Test"
                  "Some test data"
                  allContributors
                  (HM.fromList [
                      (ListID 1, list1 & L.tasks .~ (TaskID <$> [5, 3]))
                  ,   (ListID 2, list2)
                  ])
                  allListsOrder
                  (HM.fromList [
                      (TaskID 2, task2 & T.tasks .~ [] & T.related .~ (TaskID <$> [3]))
                  ,   (TaskID 3, task3 & T.related .~ (TaskID <$> [2]))
                  ,   (TaskID 4, task4)
                  ,   (TaskID 5, task5 & T.related .~ [])
                  ])

            it "no task" $ removeTasks (TaskID 99) testData `shouldBe` testData
