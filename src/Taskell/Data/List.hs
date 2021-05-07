module Taskell.Data.List (

    List (..)
,   Lists
,   ListID (..)
,   ListIDs

,   tasks
,   removeFromList
,   rename
,   new
,   addTask

) where

import RIO

import Taskell.Data.Types.List
import Taskell.Data.Task (TaskID)

-- removing tasks from lists
removeFromList :: TaskID -> Update
removeFromList taskID = tasks %~ filter (/= taskID)

rename :: Text -> Update
rename text = title .~ text

addTask :: TaskID -> Update
addTask taskID list = list & tasks %~ (<> [taskID])

new :: Text -> List
new newTitle = List newTitle []
