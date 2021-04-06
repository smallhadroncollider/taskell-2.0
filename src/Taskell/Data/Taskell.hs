module Taskell.Data.Taskell (

    TT.Taskell (..)

,   removeTasks
,   tasksForList

) where

import RIO
import qualified RIO.HashMap as HM (delete, keys, filter, lookup)

import qualified Taskell.Data.Types.Taskell as TT
import qualified Taskell.Data.List as TTL
import qualified Taskell.Data.Task as TTT

-- getting tasks
tasksForList :: TTL.ListID -> TT.Taskell -> Maybe [TTT.Task]
tasksForList listID taskell = do
    list <- listID `HM.lookup` (taskell ^. TT.lists)
    traverse (`HM.lookup` (taskell ^. TT.tasks)) (list ^. TTL.tasks)


-- removing tasks
removeFromChildren :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeFromChildren taskID taskell = updatedTasks
    where ids = HM.keys $ HM.filter (TTT.belongsToTask taskID) (taskell ^. TT.tasks)
          updatedTasks = foldl' (flip removeFromTasks) taskell ids

removeFromTasks :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeFromTasks taskID taskell = removeFromChildren taskID $ taskell & TT.tasks .~ removedImmeditate
    where removedImmeditate = TTT.removeFromTask taskID <$> HM.delete taskID (taskell ^. TT.tasks)

removeFromLists :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeFromLists taskID taskell = taskell & TT.lists .~ (TTL.removeFromList taskID <$> taskell ^. TT.lists)

removeTasks :: TTT.TaskID -> TT.Taskell -> TT.Taskell
removeTasks taskID = removeFromTasks taskID . removeFromLists taskID
