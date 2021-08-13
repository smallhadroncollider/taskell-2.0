{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

type ID = Int

-- tasks
newtype TaskID =
    TaskID ID
    deriving (Eq, Ord, Num, Show, Hashable)

type TaskIDs = Seq TaskID

-- lists
newtype ListID =
    ListID ID
    deriving (Eq, Ord, Num, Show, Hashable)

type ListIDs = Seq ListID

-- contributors
newtype ContributorID =
    ContributorID ID
    deriving (Eq, Ord, Num, Show, Hashable)

type ContributorIDs = Seq ContributorID

-- tags
newtype TagID =
    TagID ID
    deriving (Eq, Ord, Num, Show, Hashable)

type TagIDs = Seq TagID

-- functions
movePos :: Eq a => Int -> a -> Seq a -> Seq a
movePos n item list =
    case Seq.elemIndexL item list of
        Nothing -> list
        Just index -> do
            let deleted = Seq.deleteAt index list
            Seq.insertAt (index + n) item deleted

moveLeft :: Eq a => a -> Seq a -> Seq a
moveLeft = movePos (-1)

moveRight :: Eq a => a -> Seq a -> Seq a
moveRight = movePos 1

getToPos :: Eq a => Int -> a -> Seq a -> Maybe a
getToPos n item list = (list Seq.!?) . (+ n) =<< Seq.elemIndexL item list

getToLeft :: Eq a => a -> Seq a -> Maybe a
getToLeft = getToPos (-1)

getToRight :: Eq a => a -> Seq a -> Maybe a
getToRight = getToPos 1
