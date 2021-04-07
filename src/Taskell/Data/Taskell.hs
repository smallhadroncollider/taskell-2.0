module Taskell.Data.Taskell (

    TT.Taskell (..)

,   removeTasks
,   tasksForList
,   tasksForTask
,   getLists
,   moveListLeft
,   moveListRight

) where

import RIO
import qualified RIO.HashMap as HM (delete, lookup, adjust)
import qualified RIO.List as L (splitAt, span)

import qualified Taskell.Data.Types.Taskell as TT
import qualified Taskell.Data.List as TTL
import qualified Taskell.Data.Task as TTT

-- getting lists
getList :: TTL.ListID -> TT.Taskell -> Maybe TTL.List
getList listID taskell = HM.lookup listID (taskell ^. TT.lists)

getLists :: TT.Taskell -> [TTL.List]
getLists taskell = fromMaybe [] $
    traverse (`HM.lookup` (taskell ^. TT.lists)) (taskell ^. TT.listsOrder)

-- reordering lists
moveLeft :: TTL.ListID -> TTL.ListIDs -> TTL.ListIDs
moveLeft listID lists = prefix <> value <> after <> suffix
    where (before, rest) = L.span (/= listID) lists
          (value, suffix) = L.splitAt 1 rest
          (prefix, after) = L.splitAt (length before - 1) before

moveRight :: TTL.ListID -> TTL.ListIDs -> TTL.ListIDs
moveRight listID lists = prefix <> before <> value <> suffix
    where (prefix, rest) = L.span (/= listID) lists
          (value, after) = L.splitAt 1 rest
          (before, suffix) = L.splitAt 1 after

moveListLeft :: TTL.ListID -> TT.Taskell -> TT.Taskell
moveListLeft listID = TT.listsOrder %~ moveLeft listID

moveListRight :: TTL.ListID -> TT.Taskell -> TT.Taskell
moveListRight listID = TT.listsOrder %~ moveRight listID



-- getting tasks
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


-- removing tasks
removeChildren :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeChildren taskID taskell = case getTask taskID taskell of
    Just tsk -> foldl' (flip removeFromTasks) taskell (tsk ^. TTT.tasks)
    Nothing -> taskell

removeFromTasks :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeFromTasks taskID taskell = taskell & TT.tasks .~ removedImmeditate
    where removedChildren = removeChildren taskID taskell
          removed = HM.delete taskID (removedChildren ^. TT.tasks)
          removedImmeditate = TTT.removeFromTask taskID <$> removed

removeFromLists :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeFromLists taskID taskell =
    case TTT.parentList =<< getTask taskID taskell of
        Nothing -> taskell
        Just listID -> taskell & TT.lists %~ HM.adjust (TTL.removeFromList taskID) listID

removeTasks :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeTasks taskID = removeFromTasks taskID . removeFromLists taskID
