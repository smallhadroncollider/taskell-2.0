module Taskell.Data.List
    ( List(..)
    , Lists
    , ListID(..)
    , ListIDs
    , tasks
    , removeFromList
    , rename
    , new
    , addTask
    , addTaskTop
    ) where

import RIO

import Taskell.Data.Task (TaskID)
import Taskell.Data.Types.List

-- removing tasks from lists
removeFromList :: TaskID -> Update
removeFromList taskID = tasks %~ filter (/= taskID)

rename :: Text -> Update
rename text = title .~ text

addTask :: TaskID -> Update
addTask taskID list = list & tasks %~ (<> [taskID])

addTaskTop :: TaskID -> Update
addTaskTop taskID list = list & tasks %~ ([taskID] <>)

new :: Text -> List
new newTitle = List newTitle []
