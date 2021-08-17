module Groups.Task
    ( group
    ) where

import RIO

import Criterion.Main (Benchmark, bench, bgroup, whnf)

import Taskell.Data.Types.Task as T (TaskID(..))

import Taskell.Data.Taskell
    ( Taskell
    , moveTaskDown
    , moveTaskLeft
    , moveTaskRight
    , moveTaskUp
    , removeTasks
    , tasksForTask
    )

group :: Taskell -> IO Benchmark
group benchData =
    pure $
    bgroup
        "Task"
        [ bgroup
              "tasksForTasks"
              [ bench "existing task - with sub-tasks" $ whnf (tasksForTask (TaskID 1)) benchData
              , bench "existing task - without sub-tasks" $ whnf (tasksForTask (TaskID 4)) benchData
              , bench "non-existing task" $ whnf (tasksForTask (TaskID 50)) benchData
              ]
        , bgroup
              "removeTasks"
              [ bench "existing list - no relationships" $ whnf (removeTasks (TaskID 8)) benchData
              , bench "existing list - some relationships" $ whnf (removeTasks (TaskID 4)) benchData
              , bench "existing list - many relationships" $ whnf (removeTasks (TaskID 1)) benchData
              , bench "non-existing list" $ whnf (removeTasks (TaskID 50)) benchData
              ]
        , bgroup
              "moveTasks"
              [ bench "move left - change" $ whnf (moveTaskLeft (TaskID 1)) benchData
              , bench "move left - no change" $ whnf (moveTaskLeft (TaskID 7)) benchData
              , bench "move right - change" $ whnf (moveTaskRight (TaskID 7)) benchData
              , bench "move right - no change" $ whnf (moveTaskRight (TaskID 1)) benchData
              , bench "move up - change" $ whnf (moveTaskUp (TaskID 5)) benchData
              , bench "move up - no change" $ whnf (moveTaskUp (TaskID 1)) benchData
              , bench "move down - change" $ whnf (moveTaskDown (TaskID 1)) benchData
              , bench "move down - no change" $ whnf (moveTaskDown (TaskID 5)) benchData
              ]
        ]
