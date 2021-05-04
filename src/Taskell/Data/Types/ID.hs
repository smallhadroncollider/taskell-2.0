module Taskell.Data.Types.ID (

    TaskID (..)
,   TaskIDs

,   ListID (..)
,   ListIDs

,   ContributorID (..)
,   ContributorIDs

,   TagID (..)
,   TagIDs

) where

import RIO
import Data.Hashable (hashWithSalt)

type ID = Int

-- tasks
newtype TaskID = TaskID ID deriving (Eq, Show)
type TaskIDs = [TaskID]

instance Hashable TaskID where
    hashWithSalt a (TaskID b) = hashWithSalt a b


-- lists
newtype ListID = ListID ID deriving (Eq, Show)
type ListIDs = [ListID]

instance Hashable ListID where
    hashWithSalt a (ListID b) = hashWithSalt a b


-- contributors
newtype ContributorID = ContributorID ID deriving (Eq, Show)
type ContributorIDs = [ContributorID]

instance Hashable ContributorID where
    hashWithSalt a (ContributorID b) = hashWithSalt a b


-- tags
newtype TagID = TagID ID deriving (Eq, Show)
type TagIDs = [TagID]

instance Hashable TagID where
    hashWithSalt a (TagID b) = hashWithSalt a b
