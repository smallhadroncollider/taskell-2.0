module Taskell.Data.Taskell (

    TT.Taskell (..)

,   removeTasks
,   tasksForList

) where

import RIO
import qualified RIO.HashMap as HM (delete, lookup, adjust)

import qualified Taskell.Data.Types.Taskell as TT
import qualified Taskell.Data.List as TTL
import qualified Taskell.Data.Task as TTT

-- getting lists
getList :: TTL.ListID -> TT.Taskell -> Maybe TTL.List
getList listID taskell = HM.lookup listID (taskell ^. TT.lists)

-- getting tasks
tasksForList :: TTL.ListID -> TT.Taskell -> Maybe [TTT.Task]
tasksForList listID taskell = do
    list <- getList listID taskell
    traverse (`HM.lookup` (taskell ^. TT.tasks)) (list ^. TTL.tasks)

getTask :: TTT.TaskID -> TT.Taskell -> Maybe TTT.Task
getTask taskID taskell = taskID `HM.lookup` (taskell ^. TT.tasks)


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
