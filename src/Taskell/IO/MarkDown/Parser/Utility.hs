module Taskell.IO.MarkDown.Parser.Utility
    ( titleP
    ) where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

titleP :: Int -> P.Parser Text
titleP level = P.string (T.replicate level "#" <> " ") *> P.line
