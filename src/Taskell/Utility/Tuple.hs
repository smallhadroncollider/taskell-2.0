module Taskell.Utility.Tuple
    ( dup
    ) where

-- duplicate a value to both sides of a tuple
dup :: a -> (a, a)
dup a = (a, a)
