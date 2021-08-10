{-# LANGUAGE TemplateHaskell #-}

module Taskell.UI.State
    ( State(..)
    , StateReader
    , taskell
    ) where

import Import

import Lens.Micro.TH (makeLenses)

import qualified Taskell.Data.Taskell as Taskell (Taskell)

newtype State =
    State
        { _taskell :: Taskell.Taskell
        }

type StateReader = Reader State

makeLenses ''State
