module Main where

import RIO
import qualified RIO.HashMap as HM (fromList)

import Criterion.Main (defaultMain, bench, whnf, bgroup)

import Taskell.Data.Types.Contributor (Contributor (..), Contributors, ContributorID (..))
import Taskell.Data.Types.List as L (List (..), Lists, ListID (..))
import Taskell.Data.Types.Task as T (Task (..), Tasks, TaskID (..), Parent (..))

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
task1 = Task "First Task" (ParentList (ListID 1)) "Do first thing" (TaskID <$> [6]) (TaskID <$> [5]) (ContributorID <$> [1, 2]) []
task2 = Task "Second Task" (ParentList (ListID 2)) "Do second thing" [] (TaskID <$> [3, 1]) (ContributorID <$> [1]) []
task3 = Task "Third Task" (ParentList (ListID 1)) "Do third thing" [] (TaskID <$> [6, 2]) (ContributorID <$> [2]) []
task4 = Task "Fourth Task" (ParentList (ListID 2)) "Do fourth thing" [] [] (ContributorID <$> [3]) []
task5 = Task "Fifth Task" (ParentList (ListID 1)) "Do fifth thing" [] (TaskID <$> [1]) (ContributorID <$> [2]) []
task6 = Task "Sub Task" (ParentTask (TaskID 1)) "Sub task" (TaskID <$> [7]) [] [] []
task7 = Task "Sub Sub Task" (ParentTask (TaskID 6)) "Sub sub task" (TaskID <$> [8]) [] [] []
task8 = Task "Sub Sub Sub Task" (ParentTask (TaskID 7)) "Sub sub sub task" [] [] [] []

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

benchData :: Taskell
benchData = Taskell
      "Benchmark"
      "Some bench data"
      allContributors
      allLists
      [ListID 2, ListID 1]
      allTasks

main :: IO ()
main =
    defaultMain [
        bench "getLists" $ whnf getLists benchData

    ,   bgroup "moveLists" [
            bench "move left - no change" $ whnf (moveListLeft (ListID 2)) benchData
        ,   bench "move left - change" $ whnf (moveListLeft (ListID 1)) benchData
        ,   bench "move right - no change" $ whnf (moveListRight (ListID 1)) benchData
        ,   bench "move right - change" $ whnf (moveListRight (ListID 2)) benchData
        ]

    ,   bgroup "tasksForList" [
            bench "existing list" $ whnf (tasksForList (ListID 1)) benchData
        ,   bench "non-existing list" $ whnf (tasksForList (ListID 8)) benchData
        ]

    ,   bgroup "tasksForTasks" [
            bench "existing task - with sub-tasks" $ whnf (tasksForTask (TaskID 1)) benchData
        ,   bench "existing task - without sub-tasks" $ whnf (tasksForTask (TaskID 8)) benchData
        ,   bench "non-existing task" $ whnf (tasksForTask (TaskID 50)) benchData
        ]

    ,   bgroup "removeTasks" [
            bench "existing list - no relationships" $ whnf (removeTasks (TaskID 4)) benchData
        ,   bench "existing list - some relationships" $ whnf (removeTasks (TaskID 8)) benchData
        ,   bench "existing list - many relationships" $ whnf (removeTasks (TaskID 1)) benchData
        ,   bench "non-existing list" $ whnf (removeTasks (TaskID 50)) benchData
        ]
    ]
