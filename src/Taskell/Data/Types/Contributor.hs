{-# LANGUAGE TemplateHaskell #-}

module Taskell.Data.Types.Contributor
    ( Contributor(..)
    , Contributors
    , ContributorID(..)
    , ContributorIDs
    , sign
    , name
    , email
    ) where

import RIO

import Lens.Micro.TH (makeLenses)

import Taskell.Data.Types.ID (ContributorID(..), ContributorIDs)

type Contributors = HashMap ContributorID Contributor

data Contributor =
    Contributor
        { _sign :: !Text
        , _name :: !Text
        , _email :: !Text
        }
    deriving (Eq, Show)

makeLenses ''Contributor
