module Taskell.Data.Tag
    ( matches
    , create
    , addTask
    ) where

import RIO
import qualified RIO.Seq as Seq

import Taskell.Data.Types.Tag
import Taskell.Data.Types.Task (TaskID)

matches :: Text -> Tag -> Bool
matches text tag = tag ^. name == text

create :: Text -> Tag
create text = Tag text []

addTask :: TaskID -> Update
addTask taskID = tasks %~ (Seq.|> taskID)
