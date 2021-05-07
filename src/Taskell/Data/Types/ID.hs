module Taskell.Data.Types.ID (

    TaskID (..)
,   TaskIDs

,   ListID (..)
,   ListIDs

,   ContributorID (..)
,   ContributorIDs

,   TagID (..)
,   TagIDs

,   moveLeft
,   moveRight

) where

import RIO
import qualified RIO.List as L (splitAt, span)
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


-- functions
moveLeft :: Eq a => a -> [a] -> [a]
moveLeft item list = prefix <> value <> after <> suffix
    where (before, rest) = L.span (/= item) list
          (value, suffix) = L.splitAt 1 rest
          (prefix, after) = L.splitAt (length before - 1) before

moveRight :: Eq a => a -> [a] -> [a]
moveRight item list = prefix <> before <> value <> suffix
    where (prefix, rest) = L.span (/= item) list
          (value, after) = L.splitAt 1 rest
          (before, suffix) = L.splitAt 1 after
