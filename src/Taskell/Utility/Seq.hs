module Taskell.Utility.Seq
    ( appendUnique
    ) where

import RIO
import qualified RIO.Seq as Seq

appendUnique :: Eq a => a -> Seq a -> Seq a
appendUnique v s =
    case Seq.elemIndexL v s of
        Nothing -> s Seq.|> v
        Just _ -> s
