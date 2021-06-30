module Taskell.Data.Taskell
    ( TT.Taskell(..)
    , ListTuples
    , removeTasks
    , tasksForList
    , tasksForTask
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
    , renameTask
    , changeTaskDescription
    , moveTaskUp
    , moveTaskDown
    , moveTaskLeft
    , moveTaskRight
    , moveTaskLeftTop
    , moveTaskRightTop
    ) where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Seq as Seq

import qualified Taskell.Data.List as TTL
import qualified Taskell.Data.Task as TTT
import qualified Taskell.Data.Types.ID as ID
import qualified Taskell.Data.Types.List as TTL
import qualified Taskell.Data.Types.Task as TTT
import qualified Taskell.Data.Types.Taskell as TT

import qualified Taskell.Error as Error

type Result a = TT.Taskell -> Error.EitherError a

type Update = Result TT.Taskell

-- getting lists
type ListTuples = (TTL.ListID, TTL.List)

getList :: TTL.ListID -> Result TTL.List
getList listID taskell =
    Error.mEither ("Unknown reference: " <> tshow listID) (HM.lookup listID (taskell ^. TT.lists))

getListWithID :: TTL.ListID -> Result ListTuples
getListWithID listID taskell = do
    list <- getList listID taskell
    pure (listID, list)

getLists :: Result (Seq.Seq TTL.List)
getLists taskell = traverse (`getList` taskell) (taskell ^. TT.listsOrder)

getListsWithIDs :: Result (Seq.Seq ListTuples)
getListsWithIDs taskell = traverse (`getListWithID` taskell) (taskell ^. TT.listsOrder)

-- reordering lists
moveListLeft :: TTL.ListID -> Update
moveListLeft listID taskell = pure (taskell & TT.listsOrder %~ ID.moveLeft listID)

moveListRight :: TTL.ListID -> Update
moveListRight listID taskell = pure (taskell & TT.listsOrder %~ ID.moveRight listID)

-- working with lists
updateList :: TTL.Update -> TTL.ListID -> Update
updateList fn listID taskell = pure (taskell & TT.lists %~ HM.adjust fn listID)

renameList :: Text -> TTL.ListID -> Update
renameList title = updateList (TTL.rename title)

addList :: Text -> TTL.ListID -> Update
addList title listID taskell =
    pure
        (taskell & TT.lists %~ HM.insert listID (TTL.new title) &
         TT.listsOrder %~ (<> Seq.singleton listID))

removeList :: TTL.ListID -> Update
removeList listID taskell =
    pure (taskell & TT.lists %~ HM.delete listID & TT.listsOrder %~ Seq.filter (/= listID))

getListLeft :: TTL.ListID -> TT.Taskell -> Maybe TTL.ListID
getListLeft listID = ID.getToLeft listID . (^. TT.listsOrder)

getListRight :: TTL.ListID -> TT.Taskell -> Maybe TTL.ListID
getListRight listID = ID.getToRight listID . (^. TT.listsOrder)

-- getting tasks
updateTasks :: (TTT.Tasks -> TTT.Tasks) -> Update
updateTasks fn taskell = pure (taskell & TT.tasks %~ fn)

updateTask :: TTT.Update -> TTT.TaskID -> Update
updateTask fn taskID = updateTasks (HM.adjust fn taskID)

addTask :: TTT.Task -> TTT.TaskID -> Update
addTask task taskID = updateTasks (HM.insert taskID task)

taskIDsToTasks :: TTT.TaskIDs -> Result (Seq.Seq TTT.Task)
taskIDsToTasks taskIDs taskell = traverse (`getTask` taskell) taskIDs

tasksForList :: TTL.ListID -> Result (Seq.Seq TTT.Task)
tasksForList listID taskell = getList listID taskell >>= (`taskIDsToTasks` taskell) . (^. TTL.tasks)

tasksForTask :: TTT.TaskID -> Result (Seq.Seq TTT.Task)
tasksForTask taskID taskell = getTask taskID taskell >>= (`taskIDsToTasks` taskell) . (^. TTT.tasks)

getTask :: TTT.TaskID -> TT.Taskell -> Error.EitherError TTT.Task
getTask taskID taskell =
    Error.mEither ("Unknown reference: " <> tshow taskID) (HM.lookup taskID (taskell ^. TT.tasks))

addTaskToList :: Text -> TTT.TaskID -> TTL.ListID -> Update
addTaskToList title taskID listID taskell =
    addTask (TTT.new title (TTT.ParentList listID)) taskID taskell >>=
    updateList (TTL.addTask taskID) listID

renameTask :: Text -> TTT.TaskID -> Update
renameTask title = updateTask (TTT.rename title)

changeTaskDescription :: Text -> TTT.TaskID -> Update
changeTaskDescription title = updateTask (TTT.changeDescription title)

moveTaskUp :: TTT.TaskID -> Update
moveTaskUp taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentTask parentID -> updateTask (TTT.moveUp taskID) parentID taskell
        TTT.ParentList parentID -> updateList (TTT.moveUp taskID) parentID taskell

moveTaskDown :: TTT.TaskID -> Update
moveTaskDown taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentTask parentID -> updateTask (TTT.moveDown taskID) parentID taskell
        TTT.ParentList parentID -> updateList (TTT.moveDown taskID) parentID taskell

moveTaskLR ::
       (TTL.ListID -> TT.Taskell -> Maybe TTL.ListID)
    -> (TTT.TaskID -> TTL.List -> TTL.List)
    -> TTT.TaskID
    -> Update
moveTaskLR getListLR addTaskTB taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentTask _ -> pure taskell
        TTT.ParentList currentListID -> do
            case getListLR currentListID taskell of
                Nothing -> pure taskell
                Just intoID ->
                    updateList (TTL.removeFromList taskID) currentListID taskell >>=
                    updateList (addTaskTB taskID) intoID >>=
                    updateTask (TTT.setParentList intoID) taskID

moveTaskLeft :: TTT.TaskID -> Update
moveTaskLeft = moveTaskLR getListLeft TTL.addTask

moveTaskRight :: TTT.TaskID -> Update
moveTaskRight = moveTaskLR getListRight TTL.addTask

moveTaskLeftTop :: TTT.TaskID -> Update
moveTaskLeftTop = moveTaskLR getListLeft TTL.addTaskTop

moveTaskRightTop :: TTT.TaskID -> Update
moveTaskRightTop = moveTaskLR getListRight TTL.addTaskTop

-- removing tasks
removeChildren :: TTT.TaskID -> Update
removeChildren taskID taskell = do
    task <- getTask taskID taskell
    foldM (flip removeFromTasks) taskell (task ^. TTT.tasks)

removeFromTasks :: TTT.TaskID -> Update
removeFromTasks taskID taskell = do
    removedChildren <- removeChildren taskID taskell
    let removed = HM.delete taskID (removedChildren ^. TT.tasks)
    let removedImmeditate = TTT.removeFromTask taskID <$> removed
    pure (taskell & TT.tasks .~ removedImmeditate)

removeFromLists :: TTT.TaskID -> Update
removeFromLists taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentList listID -> updateList (TTL.removeFromList taskID) listID taskell
        _ -> pure taskell

removeTasks :: TTT.TaskID -> Update
removeTasks taskID taskell = removeFromLists taskID taskell >>= removeFromTasks taskID

-- Taskell
rename :: Text -> Update
rename title taskell = pure (taskell & TT.title .~ title)

changeDescription :: Text -> Update
changeDescription title taskell = pure (taskell & TT.description .~ title)
