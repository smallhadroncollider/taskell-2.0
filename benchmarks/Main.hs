module Main where

import RIO

import Criterion.Main (Benchmark, defaultMain)

import qualified Groups.IO as IO (group)
import qualified Groups.List as List (group)
import qualified Groups.Task as Task (group)
import qualified Groups.TextEditing as TextEditing (group)

import Taskell.Data.Taskell (Taskell)
import Taskell.IO.MarkDown.Convert.FromSerialized (convert)
import Taskell.IO.MarkDown.Types (defaultDictionary)

-- benchmark groups
benchmarks :: [Taskell -> IO Benchmark]
benchmarks = [IO.group, TextEditing.group, List.group, Task.group]

-- run benchmarks
getBenchData :: IO Taskell
getBenchData =
    fromRight (error "Couldn't read test data") . convert defaultDictionary <$>
    readFileUtf8 "test/data/output.md"

main :: IO ()
main = defaultMain =<< for benchmarks . (&) =<< getBenchData
