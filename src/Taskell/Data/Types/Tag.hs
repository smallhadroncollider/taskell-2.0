{-# LANGUAGE TemplateHaskell #-}

module Taskell.Data.Types.Tag
    ( Tag(..)
    , Tags
    , TagID(..)
    , TagIDs
    , name
    , tasks
    ) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.ID (TagID(..), TagIDs, TaskIDs)

data Tag =
    Tag
        { _name :: !Text
        , _tasks :: !TaskIDs
        }
    deriving (Eq, Show)

makeLenses ''Tag

type Tags = HashMap TagID Tag
