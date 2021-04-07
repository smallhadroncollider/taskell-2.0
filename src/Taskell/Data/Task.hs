module Taskell.Data.Task (

    Task (..)
,   Tasks
,   TaskID (..)
,   TaskIDs
,   Parent (..)

,   tasks
,   removeFromTask
,   belongsToTask
,   parentList

) where

import RIO

import Taskell.Data.Types.List (ListID)
import Taskell.Data.Types.Task

-- parent
belongsToTask :: TaskID -> Task -> Bool
belongsToTask taskID task =
    case task ^. parent of
        ParentTask parentID -> taskID == parentID
        _ -> False

parentList :: Task -> Maybe ListID
parentList task =
    case task ^. parent of
        ParentList parentID -> Just parentID
        _ -> Nothing


-- removing tasks from tasks
removeFromSubTasks :: TaskID -> Task -> Task
removeFromSubTasks taskID = tasks %~ filter (/= taskID)

removeFromRelated :: TaskID -> Task -> Task
removeFromRelated taskID = related %~ filter (/= taskID)

removeFromTask :: TaskID -> Task -> Task
removeFromTask taskID = removeFromSubTasks taskID . removeFromRelated taskID
