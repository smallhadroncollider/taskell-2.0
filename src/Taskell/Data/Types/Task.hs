{-# LANGUAGE TemplateHaskell #-}
module Taskell.Data.Types.Task (

    Parent (..)
,   Task (..)
,   Tasks
,   TaskID (..)
,   TaskIDs

,   tasks
,   related
,   parent

) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.ID (TaskID (..), TaskIDs, ListID, ContributorIDs, TagIDs)

data Parent = ParentTask !TaskID | ParentList !ListID deriving (Eq, Show)

data Task = Task {
        _title        :: !Text
    ,   _parent       :: !Parent
    ,   _description  :: !Text
    ,   _tasks        :: !TaskIDs
    ,   _related      :: !TaskIDs
    ,   _assigned     :: !ContributorIDs
    ,   _tags         :: !TagIDs
    } deriving (Eq, Show)

makeLenses ''Task

type Tasks = HashMap TaskID Task
