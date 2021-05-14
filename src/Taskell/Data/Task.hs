module Taskell.Data.Task
    ( Task(..)
    , Tasks
    , TaskID(..)
    , TaskIDs
    , Parent(..)
    , tasks
    , new
    , removeFromTask
    , belongsToTask
    , rename
    , changeDescription
    , setParentList
    , setParentTask
    ) where

import RIO

import Taskell.Data.Types.List (ListID)
import Taskell.Data.Types.Task

new :: Text -> Parent -> Task
new newTitle newParent = Task newTitle newParent "" [] [] [] []

rename :: Text -> Update
rename newTitle = title .~ newTitle

changeDescription :: Text -> Update
changeDescription newDescription = description .~ newDescription

-- parent
belongsToTask :: TaskID -> Task -> Bool
belongsToTask taskID task =
    case task ^. parent of
        ParentTask parentID -> taskID == parentID
        _ -> False

setParentList :: ListID -> Update
setParentList listID = parent .~ ParentList listID

setParentTask :: TaskID -> Update
setParentTask taskID = parent .~ ParentTask taskID

-- removing tasks from tasks
removeFromSubTasks :: TaskID -> Update
removeFromSubTasks taskID = tasks %~ filter (/= taskID)

removeFromRelated :: TaskID -> Update
removeFromRelated taskID = related %~ filter (/= taskID)

removeFromTask :: TaskID -> Update
removeFromTask taskID = removeFromSubTasks taskID . removeFromRelated taskID
