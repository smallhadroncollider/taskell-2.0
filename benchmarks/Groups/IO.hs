module Groups.IO
    ( group
    ) where

import RIO

import Criterion.Main (Benchmark, bench, bgroup, whnf)

import Taskell.Data.Taskell (Taskell)

import qualified Taskell.IO.MarkDown.Convert.FromSerialized as FC
import qualified Taskell.IO.MarkDown.Convert.ToSerialized as TC
import qualified Taskell.IO.MarkDown.Parser.Document as D
import qualified Taskell.IO.MarkDown.Serializer.Serialize as Ser
import Taskell.IO.MarkDown.Types (defaultDictionary)

group :: Taskell -> IO Benchmark
group benchData = do
    input <- readFileUtf8 "test/Taskell/IO/MarkDown/Parser/document.md"
    pure $
        bgroup
            "IO"
            [ bgroup
                  "Input"
                  [ bench "parses" $ whnf (D.parse defaultDictionary) input
                  , bench "reads" $ whnf (FC.convert defaultDictionary) input
                  ]
            , bgroup
                  "Output"
                  [ bench "serializes" $ whnf (Ser.serialize defaultDictionary) benchData
                  , bench "converts" $ whnf TC.convert benchData
                  ]
            ]
