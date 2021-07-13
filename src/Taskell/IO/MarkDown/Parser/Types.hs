{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Parser.Types where

import RIO

import Lens.Micro.TH (makeLenses)

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

data AlmostTask =
    AlmostTask
        { _taskTitle :: !Text
        , _taskDescription :: !(Maybe Text)
        , _taskComplete :: !Bool
        , _taskDue :: !(Maybe Text)
        , _taskTasks :: ![AlmostTask]
        , _taskTags :: ![Text]
        , _taskRelated :: ![Text]
        , _taskContributors :: ![Text]
        }
    deriving (Eq, Show)

emptyTask :: AlmostTask
emptyTask = AlmostTask "" Nothing False Nothing [] [] [] []

makeLenses ''AlmostTask

data AlmostList =
    AlmostList
        { _listTitle :: !Text
        , _listTasks :: ![AlmostTask]
        }

makeLenses ''AlmostList

data NextID =
    NextID
        { _nextTaskID :: !Int
        , _nextListID :: !Int
        }

makeLenses ''NextID

idStart :: NextID
idStart = NextID 1 1
