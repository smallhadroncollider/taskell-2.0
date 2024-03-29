{-# LANGUAGE TemplateHaskell #-}

module Taskell.Data.Types.Task
    ( HasTasks(..)
    , Parent(..)
    , Task(..)
    , Tasks
    , TaskID(..)
    , TaskIDs
    , Update
    , tasks
    , related
    , parent
    , title
    , description
    , complete
    , assigned
    , tags
    ) where

import Lens.Micro.TH (makeLenses)
import RIO

import Taskell.Data.Types.ID
    ( ContributorIDs
    , ListID
    , TagIDs
    , TaskID(..)
    , TaskIDs
    , moveLeft
    , moveRight
    )

class HasTasks a where
    moveUp :: TaskID -> a -> a
    moveDown :: TaskID -> a -> a

data Parent
    = ParentTask !TaskID
    | ParentList !ListID
    deriving (Eq, Show)

data Task =
    Task
        { _title :: !Text
        , _parent :: !Parent
        , _description :: !Text
        , _complete :: !Bool
        , _tasks :: !TaskIDs
        , _related :: !TaskIDs
        , _assigned :: !ContributorIDs
        , _tags :: !TagIDs
        }
    deriving (Eq, Show)

makeLenses ''Task

type Tasks = HashMap TaskID Task

type Update = Task -> Task

instance HasTasks Task where
    moveUp taskID = tasks %~ moveLeft taskID
    moveDown taskID = tasks %~ moveRight taskID
