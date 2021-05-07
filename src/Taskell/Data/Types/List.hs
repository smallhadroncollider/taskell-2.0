{-# LANGUAGE TemplateHaskell #-}
module Taskell.Data.Types.List (

    List (..)
,   Lists
,   ListID (..)
,   ListIDs
,   Update

,   tasks
,   title

) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.ID (ListID (..), ListIDs, TaskIDs, moveLeft, moveRight)
import Taskell.Data.Types.Task (HasTasks (..))

data List = List {
        _title  :: !Text
    ,   _tasks  :: !TaskIDs
    } deriving (Eq, Show)

makeLenses ''List

type Lists = HashMap ListID List

type Update = List -> List

instance HasTasks List where
    moveUp taskID = tasks %~ moveLeft taskID
    moveDown taskID = tasks %~ moveRight taskID
