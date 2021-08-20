module TestData where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Seq as Seq

import Taskell.Data.Taskell (Taskell(..))
import Taskell.Data.Types.Contributor (Contributor(..), ContributorID(..), Contributors)
import Taskell.Data.Types.List (List(..), ListID(..), ListIDs, Lists)
import Taskell.Data.Types.NextID (NextID, nextIDs, setContributorID, setListID, setTagID, setTaskID)
import Taskell.Data.Types.Tag (Tag(..), TagID(..), Tags)
import Taskell.Data.Types.Task (Parent(..), Task(..), TaskID(..), Tasks)

-- contributors
contributor1, contributor2, contributor3 :: Contributor
contributor1 = Contributor "Bob" "Bob" "bob@bob.com"

contributor2 = Contributor "Jenny" "Jenny" "jenny@jenny.com"

contributor3 = Contributor "Jim" "Jim" "jim@jim.com"

allContributors :: Contributors
allContributors =
    HM.fromList
        [ (ContributorID 1, contributor1)
        , (ContributorID 2, contributor2)
        , (ContributorID 3, contributor3)
        ]

-- tasks
list1Task1, list2Task1, list1Task2, list2Task2, list1Task3, subTask, subSubTask, subSubSubTask ::
       Task
list1Task1 =
    Task
        "First Task"
        (ParentList (ListID 1))
        "Do first thing"
        False
        (TaskID <$> Seq.fromList [2])
        (TaskID <$> Seq.fromList [7, 5, 6])
        (ContributorID <$> Seq.fromList [1, 2])
        [TagID 1, TagID 2]

subTask =
    Task "Sub Task" (ParentTask (TaskID 1)) "Sub task" False (TaskID <$> Seq.fromList [3]) [] [] []

subSubTask =
    Task
        "Sub Sub Task"
        (ParentTask (TaskID 2))
        ""
        True
        (TaskID <$> Seq.fromList [4])
        []
        []
        [TagID 3]

subSubSubTask = Task "Sub Sub Sub Task" (ParentTask (TaskID 3)) "Sub sub sub task" False [] [] [] []

list1Task2 =
    Task
        "Third Task"
        (ParentList (ListID 1))
        "Do third thing"
        False
        []
        (TaskID <$> Seq.fromList [1, 7])
        (ContributorID <$> Seq.fromList [2])
        [TagID 2]

list1Task3 =
    Task
        "Fifth Task"
        (ParentList (ListID 1))
        "Do fifth thing"
        False
        []
        (TaskID <$> Seq.fromList [1])
        (ContributorID <$> Seq.fromList [2])
        [TagID 3]

list2Task1 =
    Task
        "Second Task"
        (ParentList (ListID 2))
        "Do second thing"
        False
        []
        (TaskID <$> Seq.fromList [1, 5])
        (ContributorID <$> Seq.fromList [1])
        [TagID 1]

list2Task2 =
    Task
        "Fourth Task"
        (ParentList (ListID 2))
        "Do fourth thing"
        False
        []
        []
        (ContributorID <$> Seq.fromList [3])
        [TagID 3]

allTasks :: Tasks
allTasks =
    HM.fromList
        [ (TaskID 1, list1Task1)
        , (TaskID 2, subTask)
        , (TaskID 3, subSubTask)
        , (TaskID 4, subSubSubTask)
        , (TaskID 5, list1Task2)
        , (TaskID 6, list1Task3)
        , (TaskID 7, list2Task1)
        , (TaskID 8, list2Task2)
        ]

-- lists
list1, list2 :: List
list1 = List "First List" (TaskID <$> Seq.fromList [1, 5, 6])

list2 = List "Second List" (TaskID <$> Seq.fromList [7, 8])

allLists :: Lists
allLists = HM.fromList [(ListID 1, list1), (ListID 2, list2)]

allListsOrder :: ListIDs
allListsOrder = Seq.fromList [ListID 1, ListID 2]

-- tags
tag1, tag2, tag3 :: Tag
tag1 = Tag "fish" [TaskID 1, TaskID 7]

tag2 = Tag "cow" [TaskID 1, TaskID 5]

tag3 = Tag "spatula" [TaskID 3, TaskID 6, TaskID 8]

allTags :: Tags
allTags = HM.fromList [(TagID 1, tag1), (TagID 2, tag2), (TagID 3, tag3)]

-- ids
allIDs :: NextID
allIDs = setTaskID 8 . setListID 2 . setContributorID 3 . setTagID 3 $ nextIDs

-- full
testData :: Taskell
testData =
    Taskell "Test" "Some test data" allContributors allLists allListsOrder allTasks allTags allIDs
