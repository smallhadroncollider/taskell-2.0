module Taskell.Data.Taskell
    ( Taskell.Taskell(..)
    , ListTuple
    , removeTasks
    , tasksForList
    , tasksForTask
    , tasksForListWithIDs
    , getLists
    , getListsWithIDs
    , moveListLeft
    , moveListRight
    , renameList
    , rename
    , changeDescription
    , removeList
    , addList
    , addTaskToList
    , addTaskToTask
    , getTask
    , renameTask
    , changeTaskDescription
    , changeTaskCompleted
    , addTagToTask
    , setTaskContributors
    , addTaskRelationship
    , moveTaskUp
    , moveTaskDown
    , moveTaskLeft
    , moveTaskRight
    , moveTaskLeftTop
    , moveTaskRightTop
    , findContributorFromSign
    , getContributor
    , addContributor
    , getTag
    ) where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import qualified RIO.Seq as Seq

import qualified Taskell.Data.List as List
import qualified Taskell.Data.Tag as Tag
import qualified Taskell.Data.Task as Task
import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.ID as ID
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.NextID as NextID
import qualified Taskell.Data.Types.Tag as Tag
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Data.Types.Taskell as Taskell

import qualified Taskell.Error as Error

type Result a = Taskell.Taskell -> Error.EitherError a

type Update = Result Taskell.Taskell

-- getting lists
type ListTuple = (List.ListID, List.List)

getList :: List.ListID -> Result List.List
getList listID taskell =
    Error.mEither
        ("Unknown reference: " <> tshow listID)
        (HM.lookup listID (taskell ^. Taskell.lists))

getListWithID :: List.ListID -> Result ListTuple
getListWithID listID taskell = do
    list <- getList listID taskell
    pure (listID, list)

getLists :: Result (Seq List.List)
getLists taskell = traverse (`getList` taskell) (taskell ^. Taskell.listsOrder)

getListsWithIDs :: Result (Seq ListTuple)
getListsWithIDs taskell = traverse (`getListWithID` taskell) (taskell ^. Taskell.listsOrder)

-- reordering lists
moveListLeft :: List.ListID -> Update
moveListLeft listID taskell = pure (taskell & Taskell.listsOrder %~ ID.moveLeft listID)

moveListRight :: List.ListID -> Update
moveListRight listID taskell = pure (taskell & Taskell.listsOrder %~ ID.moveRight listID)

-- working with lists
updateList :: List.Update -> List.ListID -> Update
updateList fn listID taskell = pure (taskell & Taskell.lists %~ HM.adjust fn listID)

renameList :: Text -> List.ListID -> Update
renameList title = updateList (List.rename title)

addList :: Text -> Result (List.ListID, Taskell.Taskell)
addList title taskell = do
    let (listID, nids) = NextID.nextListID $ taskell ^. Taskell.nextID
    let tsk =
            taskell & Taskell.nextID .~ nids & Taskell.lists %~ HM.insert listID (List.new title) &
            Taskell.listsOrder %~ (<> Seq.singleton listID)
    pure (listID, tsk)

removeList :: List.ListID -> Update
removeList listID taskell =
    pure
        (taskell & Taskell.lists %~ HM.delete listID & Taskell.listsOrder %~ Seq.filter (/= listID))

getListLeft :: List.ListID -> Taskell.Taskell -> Maybe List.ListID
getListLeft listID = ID.getToLeft listID . (^. Taskell.listsOrder)

getListRight :: List.ListID -> Taskell.Taskell -> Maybe List.ListID
getListRight listID = ID.getToRight listID . (^. Taskell.listsOrder)

-- getting tasks
type TaskTuple = (Task.TaskID, Task.Task)

updateTasks :: (Task.Tasks -> Task.Tasks) -> Update
updateTasks fn taskell = pure (taskell & Taskell.tasks %~ fn)

updateTask :: Task.Update -> Task.TaskID -> Update
updateTask fn taskID = updateTasks (HM.adjust fn taskID)

addTask :: Task.Task -> Result (Task.TaskID, Taskell.Taskell)
addTask task taskell = do
    let (taskID, nids) = NextID.nextTaskID $ taskell ^. Taskell.nextID
    tsk <- updateTasks (HM.insert taskID task) (taskell & Taskell.nextID .~ nids)
    pure (taskID, tsk)

taskIDsToTasks :: Task.TaskIDs -> Result (Seq Task.Task)
taskIDsToTasks taskIDs taskell = traverse (`getTask` taskell) taskIDs

taskIDsToTasksWithIDs :: Task.TaskIDs -> Result (Seq TaskTuple)
taskIDsToTasksWithIDs taskIDs taskell = do
    tasks <- traverse (`getTask` taskell) taskIDs
    pure $ Seq.zip taskIDs tasks

tasksForListWithIDs :: List.ListID -> Result (Seq TaskTuple)
tasksForListWithIDs listID taskell =
    getList listID taskell >>= (`taskIDsToTasksWithIDs` taskell) . (^. List.tasks)

tasksForList :: List.ListID -> Result (Seq Task.Task)
tasksForList listID taskell =
    getList listID taskell >>= (`taskIDsToTasks` taskell) . (^. List.tasks)

tasksForTask :: Task.TaskID -> Result (Seq Task.Task)
tasksForTask taskID taskell =
    getTask taskID taskell >>= (`taskIDsToTasks` taskell) . (^. Task.tasks)

getTask :: Task.TaskID -> Taskell.Taskell -> Error.EitherError Task.Task
getTask taskID taskell =
    Error.mEither
        ("Unknown reference: " <> tshow taskID)
        (HM.lookup taskID (taskell ^. Taskell.tasks))

addTaskToList :: Text -> List.ListID -> Result (Task.TaskID, Taskell.Taskell)
addTaskToList title listID taskell = do
    (taskID, tsk) <- addTask (Task.new title (Task.ParentList listID)) taskell
    tsk' <- updateList (List.addTask taskID) listID tsk
    pure (taskID, tsk')

addTaskToTask :: Text -> Task.TaskID -> Result (Task.TaskID, Taskell.Taskell)
addTaskToTask title parentID taskell = do
    (taskID, tsk) <- addTask (Task.new title (Task.ParentTask parentID)) taskell
    tsk' <- updateTask (Task.addSubTask taskID) parentID tsk
    pure (taskID, tsk')

addTaskRelationship :: (Task.TaskID, Task.TaskID) -> Update
addTaskRelationship (tskA, tskB) tsk =
    updateTask (Task.addRelated tskA) tskB tsk >>= updateTask (Task.addRelated tskB) tskA

renameTask :: Text -> Task.TaskID -> Update
renameTask title = updateTask (Task.rename title)

changeTaskDescription :: Text -> Task.TaskID -> Update
changeTaskDescription title = updateTask (Task.changeDescription title)

changeTaskCompleted :: Bool -> Task.TaskID -> Update
changeTaskCompleted complete = updateTask (Task.changeCompleted complete)

addTagToTask :: Task.TaskID -> Text -> Update
addTagToTask taskID tag tsk = do
    let (tagID, tsk') = findOrCreateTag tag tsk
    updateTask (Task.addTag tagID) taskID tsk' >>= updateTag (Tag.addTask taskID) tagID

setTaskContributors :: Contributor.ContributorIDs -> Task.TaskID -> Update
setTaskContributors contributorIDs = updateTask (Task.setContributors contributorIDs)

moveTaskUp :: Task.TaskID -> Update
moveTaskUp taskID taskell = do
    task <- getTask taskID taskell
    case task ^. Task.parent of
        Task.ParentTask parentID -> updateTask (Task.moveUp taskID) parentID taskell
        Task.ParentList parentID -> updateList (Task.moveUp taskID) parentID taskell

moveTaskDown :: Task.TaskID -> Update
moveTaskDown taskID taskell = do
    task <- getTask taskID taskell
    case task ^. Task.parent of
        Task.ParentTask parentID -> updateTask (Task.moveDown taskID) parentID taskell
        Task.ParentList parentID -> updateList (Task.moveDown taskID) parentID taskell

moveTaskLR ::
       (List.ListID -> Taskell.Taskell -> Maybe List.ListID)
    -> (Task.TaskID -> List.List -> List.List)
    -> Task.TaskID
    -> Update
moveTaskLR getListLR addTaskTB taskID taskell = do
    task <- getTask taskID taskell
    case task ^. Task.parent of
        Task.ParentTask _ -> pure taskell
        Task.ParentList currentListID -> do
            case getListLR currentListID taskell of
                Nothing -> pure taskell
                Just intoID ->
                    updateList (List.removeFromList taskID) currentListID taskell >>=
                    updateList (addTaskTB taskID) intoID >>=
                    updateTask (Task.setParentList intoID) taskID

moveTaskLeft :: Task.TaskID -> Update
moveTaskLeft = moveTaskLR getListLeft List.addTask

moveTaskRight :: Task.TaskID -> Update
moveTaskRight = moveTaskLR getListRight List.addTask

moveTaskLeftTop :: Task.TaskID -> Update
moveTaskLeftTop = moveTaskLR getListLeft List.addTaskTop

moveTaskRightTop :: Task.TaskID -> Update
moveTaskRightTop = moveTaskLR getListRight List.addTaskTop

-- removing tasks
removeChildren :: Task.TaskID -> Update
removeChildren taskID taskell = do
    task <- getTask taskID taskell
    foldM (flip removeFromTasks) taskell (task ^. Task.tasks)

removeFromTasks :: Task.TaskID -> Update
removeFromTasks taskID taskell = do
    removedChildren <- removeChildren taskID taskell
    let removed = HM.delete taskID (removedChildren ^. Taskell.tasks)
    let removedImmeditate = Task.removeFromTask taskID <$> removed
    pure (taskell & Taskell.tasks .~ removedImmeditate)

removeFromLists :: Task.TaskID -> Update
removeFromLists taskID taskell = do
    task <- getTask taskID taskell
    case task ^. Task.parent of
        Task.ParentList listID -> updateList (List.removeFromList taskID) listID taskell
        _ -> pure taskell

removeTasks :: Task.TaskID -> Update
removeTasks taskID taskell = removeFromLists taskID taskell >>= removeFromTasks taskID

-- contributors
findContributorFromSign :: Taskell.Taskell -> Text -> Maybe Contributor.ContributorID
findContributorFromSign tsk sign = L.headMaybe matches
  where
    cs = tsk ^. Taskell.contributors
    matches = HM.keys $ HM.filter (Contributor.hasSign sign) cs

getContributor ::
       Contributor.ContributorID -> Taskell.Taskell -> Error.EitherError Contributor.Contributor
getContributor contributorID taskell =
    Error.mEither
        ("Unknown reference: " <> tshow contributorID)
        (HM.lookup contributorID (taskell ^. Taskell.contributors))

addContributor ::
       Contributor.Contributor -> Taskell.Taskell -> (Contributor.ContributorID, Taskell.Taskell)
addContributor cont taskell = (contID, tsk & Taskell.contributors %~ HM.insert contID cont)
  where
    (contID, nids) = NextID.nextContributorID (taskell ^. Taskell.nextID)
    tsk = taskell & Taskell.nextID .~ nids

--tags
updateTags :: (Tag.Tags -> Tag.Tags) -> Update
updateTags fn taskell = pure (taskell & Taskell.tags %~ fn)

updateTag :: Tag.Update -> Tag.TagID -> Update
updateTag fn taskID = updateTags (HM.adjust fn taskID)

getTag :: Tag.TagID -> Taskell.Taskell -> Error.EitherError Tag.Tag
getTag tagID taskell =
    Error.mEither ("Unknown reference: " <> tshow tagID) (HM.lookup tagID (taskell ^. Taskell.tags))

findTag :: Text -> Taskell.Taskell -> Maybe Tag.TagID
findTag text taskell =
    L.headMaybe . HM.keys $ HM.filter (Tag.matches text) (taskell ^. Taskell.tags)

addTag :: Text -> Taskell.Taskell -> (Tag.TagID, Taskell.Taskell)
addTag text taskell = (tagID, tsk & Taskell.tags %~ HM.insert tagID (Tag.create text))
  where
    (tagID, nids) = NextID.nextTagID (taskell ^. Taskell.nextID)
    tsk = taskell & Taskell.nextID .~ nids

findOrCreateTag :: Text -> Taskell.Taskell -> (Tag.TagID, Taskell.Taskell)
findOrCreateTag text tsk =
    case findTag text tsk of
        Nothing -> addTag text tsk
        Just tagID -> (tagID, tsk)

-- Taskell
rename :: Text -> Update
rename title taskell = pure (taskell & Taskell.title .~ title)

changeDescription :: Text -> Update
changeDescription title taskell = pure (taskell & Taskell.description .~ title)
