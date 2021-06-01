module Taskell.Data.TestData where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Seq as Seq

import Taskell.Data.Taskell (Taskell(..))
import Taskell.Data.Types.Contributor (Contributor(..), ContributorID(..), Contributors)
import Taskell.Data.Types.List as L (List(..), ListID(..), ListIDs, Lists)
import Taskell.Data.Types.Task as T (Parent(..), Task(..), TaskID(..), Tasks)

-- contributors
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

-- tasks
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

-- lists
list1, list2 :: List
list1 = List "First List" (TaskID <$> Seq.fromList [1, 5, 3])

list2 = List "Second List" (TaskID <$> Seq.fromList [2, 4])

allLists :: Lists
allLists = HM.fromList [(ListID 1, list1), (ListID 2, list2)]

allListsOrder :: ListIDs
allListsOrder = Seq.fromList [ListID 2, ListID 1]

-- full
testData :: Taskell
testData = Taskell "Test" "Some test data" allContributors allLists allListsOrder allTasks
