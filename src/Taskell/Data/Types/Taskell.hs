{-# LANGUAGE TemplateHaskell #-}

module Taskell.Data.Types.Taskell
    ( Taskell(..)
    , title
    , description
    , contributors
    , lists
    , listsOrder
    , tasks
    , tags
    , create
    , nextID
    ) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.Contributor (Contributors)
import Taskell.Data.Types.List (ListIDs, Lists)
import Taskell.Data.Types.NextID (NextID, nextIDs)
import Taskell.Data.Types.Tag (Tags)
import Taskell.Data.Types.Task (Tasks)

data Taskell =
    Taskell
        { _title :: !Text
        , _description :: !Text
        , _contributors :: !Contributors
        , _lists :: !Lists
        , _listsOrder :: !ListIDs
        , _tasks :: !Tasks
        , _tags :: !Tags
        , _nextID :: !NextID
        }
    deriving (Eq, Show)

makeLenses ''Taskell

create :: Text -> Taskell
create tle = Taskell tle "" [] [] [] [] [] nextIDs
