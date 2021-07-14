{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Parser.Types where

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

-- parsed data
data ParsedTask =
    ParsedTask
        { _taskTitle :: !Text
        , _taskDescription :: !(Maybe Text)
        , _taskComplete :: !Bool
        , _taskDue :: !(Maybe Text)
        , _taskTasks :: ![ParsedTask]
        , _taskTags :: ![Text]
        , _taskRelated :: ![Text]
        , _taskContributors :: ![Text]
        }
    deriving (Eq, Show)

emptyTask :: ParsedTask
emptyTask = ParsedTask "" Nothing False Nothing [] [] [] []

makeLenses ''ParsedTask

data ParsedList =
    ParsedList
        { _listTitle :: !Text
        , _listTasks :: ![ParsedTask]
        }
    deriving (Eq, Show)

makeLenses ''ParsedList

data ParsedContributor =
    ParsedContributor
        { _contributorSign :: !Text
        , _contributorName :: !Text
        , _contributorEmail :: !Text
        }
    deriving (Eq, Show)

makeLenses ''ParsedContributor

data ParsedTaskell =
    ParsedTaskell
        { _taskellTitle :: !Text
        , _taskellDescription :: !Text
        , _taskellContributors :: ![ParsedContributor]
        , _taskellLists :: ![ParsedList]
        }
    deriving (Eq, Show)

makeLenses ''ParsedTaskell
