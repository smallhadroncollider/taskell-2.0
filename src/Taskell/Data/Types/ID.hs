module Taskell.Data.Types.ID
    ( TaskID(..)
    , TaskIDs
    , ListID(..)
    , ListIDs
    , ContributorID(..)
    , ContributorIDs
    , TagID(..)
    , TagIDs
    , moveLeft
    , moveRight
    , getToLeft
    , getToRight
    ) where

import RIO
import qualified RIO.Seq as Seq

import Data.Hashable (hashWithSalt)

type ID = Int

-- tasks
newtype TaskID =
    TaskID ID
    deriving (Eq, Show)

type TaskIDs = Seq.Seq TaskID

instance Hashable TaskID where
    hashWithSalt a (TaskID b) = hashWithSalt a b

-- lists
newtype ListID =
    ListID ID
    deriving (Eq, Show)

type ListIDs = Seq.Seq ListID

instance Hashable ListID where
    hashWithSalt a (ListID b) = hashWithSalt a b

-- contributors
newtype ContributorID =
    ContributorID ID
    deriving (Eq, Show)

type ContributorIDs = Seq.Seq ContributorID

instance Hashable ContributorID where
    hashWithSalt a (ContributorID b) = hashWithSalt a b

-- tags
newtype TagID =
    TagID ID
    deriving (Eq, Show)

type TagIDs = Seq.Seq TagID

instance Hashable TagID where
    hashWithSalt a (TagID b) = hashWithSalt a b

-- functions
movePos :: Eq a => Int -> a -> Seq.Seq a -> Seq.Seq a
movePos n item list =
    case Seq.elemIndexL item list of
        Nothing -> list
        Just index -> do
            let deleted = Seq.deleteAt index list
            Seq.insertAt (index + n) item deleted

moveLeft :: Eq a => a -> Seq.Seq a -> Seq.Seq a
moveLeft = movePos (-1)

moveRight :: Eq a => a -> Seq.Seq a -> Seq.Seq a
moveRight = movePos 1

getToPos :: Eq a => Int -> a -> Seq.Seq a -> Maybe a
getToPos n item list = (list Seq.!?) . (+ n) =<< Seq.elemIndexL item list

getToLeft :: Eq a => a -> Seq.Seq a -> Maybe a
getToLeft = getToPos (-1)

getToRight :: Eq a => a -> Seq.Seq a -> Maybe a
getToRight = getToPos 1
