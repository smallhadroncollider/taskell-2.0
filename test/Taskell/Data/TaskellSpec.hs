module Taskell.Data.TaskellSpec (spec) where

import RIO
import qualified RIO.HashMap as HM (fromList)

import Test.Hspec

import Taskell.Data.Types.Contributor (Contributor (..), Contributors, ContributorID (..))
import Taskell.Data.Types.List as L (List (..), Lists, ListID (..), tasks)
import Taskell.Data.Types.Task as T (Task (..), Tasks, TaskID (..), Parent (..), tasks, related)
import Taskell.Data.Types.Taskell (listsOrder)

import Taskell.Data.Taskell (Taskell (..), tasksForList, tasksForTask, removeTasks, getLists, moveListLeft, moveListRight)


-- test data
contributor1, contributor2, contributor3 :: Contributor
contributor1 = Contributor "Bob" "bob@bob.com"
contributor2 = Contributor "Jim" "jim@jim.com"
contributor3 = Contributor "Jenny" "jenny@jenny.com"

allContributors :: Contributors
allContributors = HM.fromList [ (ContributorID 1, contributor1) , (ContributorID 2, contributor2) , (ContributorID 3, contributor3) ]

list1, list2 :: List
list1 = List "First List" (TaskID <$> [1, 5, 3])
list2 = List "Second List" (TaskID <$> [2, 4])

allLists :: Lists
allLists = HM.fromList [ (ListID 1, list1) , (ListID 2, list2) ]

task1, task2, task3, task4, task5, task6, task7, task8 :: Task
task1 = Task "First Task" (ParentList (ListID 1)) "Do first thing" (TaskID <$> [6]) (TaskID <$> [5]) (ContributorID <$> [1, 2])
task2 = Task "Second Task" (ParentList (ListID 2)) "Do second thing" [] (TaskID <$> [3, 1]) (ContributorID <$> [1])
task3 = Task "Third Task" (ParentList (ListID 1)) "Do third thing" [] (TaskID <$> [6, 2]) (ContributorID <$> [2])
task4 = Task "Fourth Task" (ParentList (ListID 2)) "Do fourth thing" [] [] (ContributorID <$> [3])
task5 = Task "Fifth Task" (ParentList (ListID 1)) "Do fifth thing" [] (TaskID <$> [1]) (ContributorID <$> [2])
task6 = Task "Sub Task" (ParentTask (TaskID 1)) "Sub task" (TaskID <$> [7]) [] []
task7 = Task "Sub Sub Task" (ParentTask (TaskID 6)) "Sub sub task" (TaskID <$> [8]) [] []
task8 = Task "Sub Sub Sub Task" (ParentTask (TaskID 7)) "Sub sub sub task" [] [] []

allTasks :: Tasks
allTasks = HM.fromList [
          (TaskID 1, task1)
      ,   (TaskID 2, task2)
      ,   (TaskID 3, task3)
      ,   (TaskID 4, task4)
      ,   (TaskID 5, task5)
      ,   (TaskID 6, task6)
      ,   (TaskID 7, task7)
      ,   (TaskID 8, task8)
      ]

testData :: Taskell
testData = Taskell
      "Test"
      "Some test data"
      allContributors
      allLists
      [ListID 2, ListID 1]
      allTasks


-- tests
spec :: Spec
spec = do
    describe "Data" $ do
        it "gets lists" $ getLists testData `shouldBe` [list2, list1]

        describe "reorders lists" $ do
            it "list 1 left" $ moveListLeft (ListID 1) testData `shouldBe` (testData & listsOrder .~ [ListID 1, ListID 2])
            it "list 2 left" $ moveListLeft (ListID 2) testData `shouldBe` (testData & listsOrder .~ [ListID 2, ListID 1])
            it "list 1 right" $ moveListRight (ListID 1) testData `shouldBe` (testData & listsOrder .~ [ListID 2, ListID 1])
            it "list 2 right" $ moveListRight (ListID 2) testData `shouldBe` (testData & listsOrder .~ [ListID 1, ListID 2])

        describe "gets tasks for lists" $ do
            it "list 1" $ tasksForList (ListID 1) testData `shouldBe` [task1, task5, task3]
            it "list 2" $ tasksForList (ListID 2) testData `shouldBe` [task2, task4]
            it "no list" $ tasksForList (ListID 3) testData `shouldBe` []

        describe "gets tasks for tasks" $ do
            it "task 1" $ tasksForTask (TaskID 1) testData `shouldBe` [task6]
            it "task 6" $ tasksForTask (TaskID 6) testData `shouldBe` [task7]
            it "task 7" $ tasksForTask (TaskID 7) testData `shouldBe` [task8]
            it "task 8" $ tasksForTask (TaskID 8) testData `shouldBe` []

        describe "removes tasks" $ do
            it "task 1" $ removeTasks (TaskID 1) testData `shouldBe` Taskell
                  "Test"
                  "Some test data"
                  allContributors
                  (HM.fromList [
                      (ListID 1, list1 & L.tasks .~ (TaskID <$> [5, 3]))
                  ,   (ListID 2, list2)
                  ])
                  [ListID 2, ListID 1]
                  (HM.fromList [
                      (TaskID 2, task2 & T.tasks .~ [] & T.related .~ (TaskID <$> [3]))
                  ,   (TaskID 3, task3 & T.related .~ (TaskID <$> [2]))
                  ,   (TaskID 4, task4)
                  ,   (TaskID 5, task5 & T.related .~ [])
                  ])

            it "no task" $ removeTasks (TaskID 99) testData `shouldBe` testData
