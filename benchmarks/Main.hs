module Main where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Seq as Seq

import Criterion.Main (bench, bgroup, defaultMain, whnf)

import Taskell.Data.Types.Contributor (Contributor(..), ContributorID(..), Contributors)
import Taskell.Data.Types.List as L (List(..), ListID(..), Lists)
import Taskell.Data.Types.Task as T (Parent(..), Task(..), TaskID(..), Tasks)

import Taskell.Data.Taskell
    ( Taskell(..)
    , getLists
    , moveListLeft
    , moveListRight
    , moveTaskDown
    , moveTaskLeft
    , moveTaskRight
    , moveTaskUp
    , removeTasks
    , tasksForList
    , tasksForTask
    )

-- test data
contributor1, contributor2, contributor3 :: Contributor
contributor1 = Contributor "Bob" "bob@bob.com"

contributor2 = Contributor "Jim" "jim@jim.com"

contributor3 = Contributor "Jenny" "jenny@jenny.com"

allContributors :: Contributors
allContributors =
    HM.fromList
        [ (ContributorID 1, contributor1)
        , (ContributorID 2, contributor2)
        , (ContributorID 3, contributor3)
        ]

list1, list2 :: List
list1 = List "First List" (TaskID <$> Seq.fromList [1, 5, 3])

list2 = List "Second List" (TaskID <$> Seq.fromList [2, 4])

allLists :: Lists
allLists = HM.fromList [(ListID 1, list1), (ListID 2, list2)]

task1, task2, task3, task4, task5, task6, task7, task8 :: Task
task1 =
    Task
        "First Task"
        (ParentList (ListID 1))
        "Do first thing"
        (TaskID <$> Seq.fromList [6])
        (TaskID <$> Seq.fromList [5])
        (ContributorID <$> Seq.fromList [1, 2])
        Seq.empty

task2 =
    Task
        "Second Task"
        (ParentList (ListID 2))
        "Do second thing"
        Seq.empty
        (TaskID <$> Seq.fromList [3, 1])
        (ContributorID <$> Seq.fromList [1])
        Seq.empty

task3 =
    Task
        "Third Task"
        (ParentList (ListID 1))
        "Do third thing"
        Seq.empty
        (TaskID <$> Seq.fromList [6, 2])
        (ContributorID <$> Seq.fromList [2])
        Seq.empty

task4 =
    Task
        "Fourth Task"
        (ParentList (ListID 2))
        "Do fourth thing"
        Seq.empty
        Seq.empty
        (ContributorID <$> Seq.fromList [3])
        Seq.empty

task5 =
    Task
        "Fifth Task"
        (ParentList (ListID 1))
        "Do fifth thing"
        Seq.empty
        (TaskID <$> Seq.fromList [1])
        (ContributorID <$> Seq.fromList [2])
        Seq.empty

task6 =
    Task
        "Sub Task"
        (ParentTask (TaskID 1))
        "Sub task"
        (TaskID <$> Seq.fromList [7])
        Seq.empty
        Seq.empty
        Seq.empty

task7 =
    Task
        "Sub Sub Task"
        (ParentTask (TaskID 6))
        "Sub sub task"
        (TaskID <$> Seq.fromList [8])
        Seq.empty
        Seq.empty
        Seq.empty

task8 =
    Task
        "Sub Sub Sub Task"
        (ParentTask (TaskID 7))
        "Sub sub sub task"
        Seq.empty
        Seq.empty
        Seq.empty
        Seq.empty

allTasks :: Tasks
allTasks =
    HM.fromList
        [ (TaskID 1, task1)
        , (TaskID 2, task2)
        , (TaskID 3, task3)
        , (TaskID 4, task4)
        , (TaskID 5, task5)
        , (TaskID 6, task6)
        , (TaskID 7, task7)
        , (TaskID 8, task8)
        ]

benchData :: Taskell
benchData =
    Taskell
        "Benchmark"
        "Some bench data"
        allContributors
        allLists
        (ListID <$> Seq.fromList [2, 1])
        allTasks

main :: IO ()
main =
    defaultMain
        [ bench "getLists" $ whnf getLists benchData
        , bgroup
              "moveLists"
              [ bench "move left - no change" $ whnf (moveListLeft (ListID 2)) benchData
              , bench "move left - change" $ whnf (moveListLeft (ListID 1)) benchData
              , bench "move right - no change" $ whnf (moveListRight (ListID 1)) benchData
              , bench "move right - change" $ whnf (moveListRight (ListID 2)) benchData
              ]
        , bgroup
              "tasksForList"
              [ bench "existing list" $ whnf (tasksForList (ListID 1)) benchData
              , bench "non-existing list" $ whnf (tasksForList (ListID 8)) benchData
              ]
        , bgroup
              "tasksForTasks"
              [ bench "existing task - with sub-tasks" $ whnf (tasksForTask (TaskID 1)) benchData
              , bench "existing task - without sub-tasks" $ whnf (tasksForTask (TaskID 8)) benchData
              , bench "non-existing task" $ whnf (tasksForTask (TaskID 50)) benchData
              ]
        , bgroup
              "removeTasks"
              [ bench "existing list - no relationships" $ whnf (removeTasks (TaskID 4)) benchData
              , bench "existing list - some relationships" $ whnf (removeTasks (TaskID 8)) benchData
              , bench "existing list - many relationships" $ whnf (removeTasks (TaskID 1)) benchData
              , bench "non-existing list" $ whnf (removeTasks (TaskID 50)) benchData
              ]
        , bgroup
              "moveTasks"
              [ bench "move left - change" $ whnf (moveTaskLeft (TaskID 1)) benchData
              , bench "move left - no change" $ whnf (moveTaskLeft (TaskID 2)) benchData
              , bench "move right - change" $ whnf (moveTaskRight (TaskID 2)) benchData
              , bench "move right - no change" $ whnf (moveTaskRight (TaskID 1)) benchData
              , bench "move up - change" $ whnf (moveTaskUp (TaskID 3)) benchData
              , bench "move up - no change" $ whnf (moveTaskUp (TaskID 1)) benchData
              , bench "move down - change" $ whnf (moveTaskDown (TaskID 1)) benchData
              , bench "move down - no change" $ whnf (moveTaskDown (TaskID 3)) benchData
              ]
        ]
