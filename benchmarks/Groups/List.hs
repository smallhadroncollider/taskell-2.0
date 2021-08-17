module Groups.List
    ( group
    ) where

import RIO

import Criterion.Main (Benchmark, bench, bgroup, whnf)

import Taskell.Data.Types.List as L (ListID(..))

import Taskell.Data.Taskell (Taskell, getLists, moveListLeft, moveListRight, tasksForList)

group :: Taskell -> IO Benchmark
group benchData =
    pure $
    bgroup
        "List"
        [ bench "getLists" $ whnf getLists benchData
        , bgroup
              "moveLists"
              [ bench "move left - no change" $ whnf (moveListLeft (ListID 1)) benchData
              , bench "move left - change" $ whnf (moveListLeft (ListID 2)) benchData
              , bench "move right - no change" $ whnf (moveListRight (ListID 2)) benchData
              , bench "move right - change" $ whnf (moveListRight (ListID 1)) benchData
              ]
        , bgroup
              "tasksForList"
              [ bench "existing list" $ whnf (tasksForList (ListID 1)) benchData
              , bench "non-existing list" $ whnf (tasksForList (ListID 8)) benchData
              ]
        ]
