{-# LANGUAGE TemplateHaskell #-}
module Taskell.Data.Types.Taskell (

    Taskell (..)

,   lists
,   tasks
,   listsOrder

) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.Contributor (Contributors)
import Taskell.Data.Types.List (Lists, ListIDs)
import Taskell.Data.Types.Task (Tasks)

data Taskell = Taskell {
        _title        :: !Text
    ,   _description  :: !Text
    ,   _contributors :: !Contributors
    ,   _lists        :: !Lists
    ,   _listsOrder   :: !ListIDs
    ,   _tasks        :: !Tasks
    } deriving (Eq, Show)

makeLenses ''Taskell
