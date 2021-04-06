{-# LANGUAGE TemplateHaskell #-}
module Taskell.Data.Types.List (

    List (..)
,   Lists
,   ListID (..)
,   ListIDs

,   tasks

) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.ID (ListID (..), ListIDs, TaskIDs)

data List = List {
        _title  :: !Text
    ,   _tasks  :: !TaskIDs
    } deriving (Eq, Show)

makeLenses ''List

type Lists = HashMap ListID List
