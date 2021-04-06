module Taskell.Data.Task (

    Task (..)
,   Tasks
,   TaskID (..)
,   TaskIDs
,   Parent (..)

,   removeFromTask
,   belongsToTask

) where

import RIO

import Taskell.Data.Types.Task

-- tasks
belongsToTask :: TaskID -> Task -> Bool
belongsToTask taskID task =
    case task ^. parent of
        ParentTask parentID -> taskID == parentID
        _ -> False

-- removing tasks from tasks
removeFromSubTasks :: TaskID -> Task -> Task
removeFromSubTasks taskID = tasks %~ filter (/= taskID)

removeFromRelated :: TaskID -> Task -> Task
removeFromRelated taskID = related %~ filter (/= taskID)

removeFromTask :: TaskID -> Task -> Task
removeFromTask taskID = removeFromSubTasks taskID . removeFromRelated taskID
