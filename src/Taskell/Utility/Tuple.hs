module Taskell.Utility.Tuple
    ( dup
    , thrd
    ) where

-- duplicate a value to both sides of a tuple
dup :: a -> (a, a)
dup a = (a, a)

thrd :: (a, b, c) -> c
thrd (_, _, c) = c
