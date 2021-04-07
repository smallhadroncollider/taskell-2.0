module Taskell.Data.List (

    List (..)
,   Lists
,   ListID (..)
,   ListIDs

,   tasks
,   removeFromList

) where

import RIO

import Taskell.Data.Types.List
import Taskell.Data.Task (TaskID)

-- removing tasks from lists
removeFromList :: TaskID -> List -> List
removeFromList taskID = tasks %~ filter (/= taskID)

