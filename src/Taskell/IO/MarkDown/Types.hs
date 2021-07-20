{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Types where

import RIO

import Lens.Micro.TH (makeLenses)

-- configurable parts of format
data Dictionary =
    Dictionary
        { _contributorsTitle :: !Text
        , _duePrefix :: !Text
        , _relatedPrefix :: !Text
        , _contributorsPrefix :: !Text
        }

makeLenses ''Dictionary

defaultDictionary :: Dictionary
defaultDictionary = Dictionary "Contributors" "**Due**:" "**Related**:" "**Contributors**:"

-- intermediary data format
data SerializedTask =
    SerializedTask
        { _taskTitle :: !Text
        , _taskDescription :: !(Maybe Text)
        , _taskComplete :: !Bool
        , _taskDue :: !(Maybe Text)
        , _taskTasks :: ![SerializedTask]
        , _taskTags :: ![Text]
        , _taskRelated :: ![Text]
        , _taskContributors :: ![Text]
        }
    deriving (Eq, Show)

emptyTask :: SerializedTask
emptyTask = SerializedTask "" Nothing False Nothing [] [] [] []

makeLenses ''SerializedTask

data SerializedList =
    SerializedList
        { _listTitle :: !Text
        , _listTasks :: ![SerializedTask]
        }
    deriving (Eq, Show)

makeLenses ''SerializedList

data SerializedContributor =
    SerializedContributor
        { _contributorSign :: !Text
        , _contributorName :: !Text
        , _contributorEmail :: !Text
        }
    deriving (Eq, Show)

makeLenses ''SerializedContributor

data SerializedTaskell =
    SerializedTaskell
        { _taskellTitle :: !Text
        , _taskellDescription :: !Text
        , _taskellContributors :: ![SerializedContributor]
        , _taskellLists :: ![SerializedList]
        }
    deriving (Eq, Show)

makeLenses ''SerializedTaskell
