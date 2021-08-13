{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Taskell.Data.Types.NextID
    ( NextID
    , nextIDs
    , nextTaskID
    , nextContributorID
    , nextListID
    , nextTagID
    , setTaskID
    , setContributorID
    , setListID
    , setTagID
    ) where

import RIO

import Taskell.Data.Types.ID (ContributorID(..), ListID(..), TagID(..), TaskID(..))
import Taskell.Utility.Tuple (dup)

import Lens.Micro ((+~))
import Lens.Micro.TH (makeLenses)

-- ID tracking
data NextID =
    NextID
        { _nTaskID :: !TaskID
        , _nListID :: !ListID
        , _nContributorID :: !ContributorID
        , _nTagID :: !TagID
        }
    deriving (Show, Eq)

makeLenses ''NextID

next :: Num a => Lens' NextID a -> NextID -> (a, NextID)
next lns = first (^. lns) . dup . (lns +~ 1)

-- exported
nextIDs :: NextID
nextIDs = NextID 0 0 0 0

nextTaskID :: NextID -> (TaskID, NextID)
nextTaskID = next nTaskID

nextListID :: NextID -> (ListID, NextID)
nextListID = next nListID

nextTagID :: NextID -> (TagID, NextID)
nextTagID = next nTagID

nextContributorID :: NextID -> (ContributorID, NextID)
nextContributorID = next nContributorID

setTaskID :: Int -> NextID -> NextID
setTaskID tid = nTaskID .~ TaskID tid

setListID :: Int -> NextID -> NextID
setListID tid = nListID .~ ListID tid

setContributorID :: Int -> NextID -> NextID
setContributorID tid = nContributorID .~ ContributorID tid

setTagID :: Int -> NextID -> NextID
setTagID tid = nTagID .~ TagID tid
