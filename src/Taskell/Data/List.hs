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
import qualified RIO.Seq as Seq

import Taskell.Data.Task (TaskID)
import Taskell.Data.Types.List

-- removing tasks from lists
removeFromList :: TaskID -> Update
removeFromList taskID = tasks %~ Seq.filter (/= taskID)

rename :: Text -> Update
rename text = title .~ text

addTask :: TaskID -> Update
addTask taskID list = list & tasks %~ (<> Seq.singleton taskID)

addTaskTop :: TaskID -> Update
addTaskTop taskID list = list & tasks %~ (Seq.singleton taskID <>)

new :: Text -> List
new newTitle = List newTitle []
