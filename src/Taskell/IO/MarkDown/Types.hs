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
