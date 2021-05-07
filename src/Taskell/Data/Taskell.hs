module Taskell.Data.Taskell (

    TT.Taskell (..)

,   removeTasks
,   tasksForList
,   tasksForTask
,   getLists
,   moveListLeft
,   moveListRight
,   renameList
,   rename
,   changeDescription
,   removeList
,   addList
,   addTaskToList
,   renameTask
,   changeTaskDescription
,   moveTaskUp
,   moveTaskDown

) where

import RIO
import qualified RIO.HashMap as HM (delete, lookup, adjust, insert)

import qualified Taskell.Data.Types.ID as ID
import qualified Taskell.Data.Types.Taskell as TT
import qualified Taskell.Data.Types.List as TTL
import qualified Taskell.Data.List as TTL
import qualified Taskell.Data.Types.Task as TTT
import qualified Taskell.Data.Task as TTT

type Update = TT.Taskell -> TT.Taskell

-- getting lists
getList :: TTL.ListID -> TT.Taskell -> Maybe TTL.List
getList listID taskell = HM.lookup listID (taskell ^. TT.lists)

getLists :: TT.Taskell -> [TTL.List]
getLists taskell = fromMaybe [] $
    traverse (`HM.lookup` (taskell ^. TT.lists)) (taskell ^. TT.listsOrder)

-- reordering lists
moveListLeft :: TTL.ListID -> Update
moveListLeft listID = TT.listsOrder %~ ID.moveLeft listID

moveListRight :: TTL.ListID -> Update
moveListRight listID = TT.listsOrder %~ ID.moveRight listID


-- working with lists
updateList :: TTL.Update -> TTL.ListID -> Update
updateList fn listID taskell =
    case fn <$> getList listID taskell of
        Nothing -> taskell
        Just list -> taskell & TT.lists %~ HM.insert listID list

renameList :: Text -> TTL.ListID -> Update
renameList title = updateList (TTL.rename title)

addList :: Text -> TTL.ListID -> Update
addList title listID taskell = taskell
    & TT.lists %~ HM.insert listID (TTL.new title)
    & TT.listsOrder %~ (<> [listID])

removeList :: TTL.ListID -> Update
removeList listID taskell = taskell
    & TT.lists %~ HM.delete listID
    & TT.listsOrder %~ filter (/= listID)



-- getting tasks
updateTask :: TTT.Update -> TTT.TaskID -> Update
updateTask fn taskID taskell =
    case fn <$> getTask taskID taskell of
        Nothing -> taskell
        Just task -> taskell & TT.tasks %~ HM.insert taskID task

taskIDsToTasks :: TT.Taskell -> TTT.TaskIDs -> Maybe [TTT.Task]
taskIDsToTasks taskell = traverse (`HM.lookup` (taskell ^. TT.tasks))

tasksForList :: TTL.ListID -> TT.Taskell -> [TTT.Task]
tasksForList listID taskell = fromMaybe [] $
    getList listID taskell >>= taskIDsToTasks taskell . (^. TTL.tasks)

tasksForTask :: TTT.TaskID -> TT.Taskell -> [TTT.Task]
tasksForTask taskID taskell = fromMaybe [] $
    getTask taskID taskell >>= taskIDsToTasks taskell . (^. TTT.tasks)

getTask :: TTT.TaskID -> TT.Taskell -> Maybe TTT.Task
getTask taskID taskell = HM.lookup taskID (taskell ^. TT.tasks)

addTaskToList :: Text -> TTT.TaskID -> TTL.ListID -> Update
addTaskToList title taskID listID taskell = updateList (TTL.addTask taskID) listID taskell
    & TT.tasks %~ HM.insert taskID (TTT.new title (TTT.ParentList listID))

renameTask :: Text -> TTT.TaskID -> Update
renameTask title = updateTask (TTT.rename title)

changeTaskDescription :: Text -> TTT.TaskID -> Update
changeTaskDescription title = updateTask (TTT.changeDescription title)

moveTaskUp :: TTT.TaskID -> Update
moveTaskUp taskID taskell = fromMaybe taskell $ do
    task <- getTask taskID taskell
    if TTT.parentIsList task
        then do
            parentID <- TTT.parentList task
            pure $ updateList (TTT.moveUp taskID) parentID taskell
        else do
            parentID <- TTT.parentTask task
            pure $ updateTask (TTT.moveUp taskID) parentID taskell

moveTaskDown :: TTT.TaskID -> Update
moveTaskDown taskID taskell = fromMaybe taskell $ do
    task <- getTask taskID taskell
    if TTT.parentIsList task
        then do
            parentID <- TTT.parentList task
            pure $ updateList (TTT.moveDown taskID) parentID taskell
        else do
            parentID <- TTT.parentTask task
            pure $ updateTask (TTT.moveDown taskID) parentID taskell


-- removing tasks
removeChildren :: TTT.TaskID -> Update
removeChildren taskID taskell = case getTask taskID taskell of
    Just tsk -> foldl' (flip removeFromTasks) taskell (tsk ^. TTT.tasks)
    Nothing -> taskell

removeFromTasks :: TTT.TaskID -> Update
removeFromTasks taskID taskell = taskell & TT.tasks .~ removedImmeditate
    where removedChildren = removeChildren taskID taskell
          removed = HM.delete taskID (removedChildren ^. TT.tasks)
          removedImmeditate = TTT.removeFromTask taskID <$> removed

removeFromLists :: TTT.TaskID -> Update
removeFromLists taskID taskell =
    case TTT.parentList =<< getTask taskID taskell of
        Nothing -> taskell
        Just listID -> taskell & TT.lists %~ HM.adjust (TTL.removeFromList taskID) listID

removeTasks :: TTT.TaskID -> Update
removeTasks taskID = removeFromTasks taskID . removeFromLists taskID

-- Taskell
rename :: Text -> Update
rename title = TT.title .~ title

changeDescription :: Text -> Update
changeDescription title = TT.description .~ title
