{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Convert.ToSerialized
    ( convert
    ) where

import RIO
import qualified RIO.List as L

import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.Taskell as Taskell

import Taskell.IO.MarkDown.Types

contributorS :: Contributor.Contributor -> SerializedContributor
contributorS cont =
    SerializedContributor
        (cont ^. Contributor.sign)
        (cont ^. Contributor.name)
        (cont ^. Contributor.email)

contributorsS :: Contributor.Contributors -> [SerializedContributor]
contributorsS conts = contributorS <$> conts'
  where
    conts' = L.sortOn (^. Contributor.sign) $ toList conts

convert :: Taskell.Taskell -> SerializedTaskell
convert tsk = SerializedTaskell title description contributors lists
  where
    title = tsk ^. Taskell.title
    description = tsk ^. Taskell.description
    contributors = contributorsS $ tsk ^. Taskell.contributors
    lists = []
