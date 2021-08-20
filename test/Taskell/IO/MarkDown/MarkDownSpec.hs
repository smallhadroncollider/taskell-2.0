module Taskell.IO.MarkDown.MarkDownSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.Error (txt)
import Taskell.IO.MarkDown.Convert.FromSerialized as From (convert)
import Taskell.IO.MarkDown.Convert.ToSerialized as To (convert)
import Taskell.IO.MarkDown.Parser.Document (parse)
import Taskell.IO.MarkDown.Serializer.Serialize (serialize)
import Taskell.IO.MarkDown.Types (defaultDictionary)

import TestData

-- tests
spec :: Spec
spec =
    parallel $ do
        describe "reads and writes" $ do
            it "gives same SerializedTaskell structure in then out" $ do
                file <- readFileUtf8 "test/data/output.md"
                let intIn = first txt $ parse defaultDictionary file
                let intOut = To.convert =<< From.convert defaultDictionary file
                intOut `shouldBe` intIn
            it "parse then serialize" $ do
                file <- readFileUtf8 "test/data/output.md"
                let tsk = From.convert defaultDictionary file
                let serialized = utf8BuilderToText <$> (serialize defaultDictionary =<< tsk)
                serialized `shouldBe` Right file
            it "serialize then parse" $ do
                let serialized = serialize defaultDictionary testData
                let tsk = From.convert defaultDictionary . utf8BuilderToText =<< serialized
                tsk `shouldBe` Right testData
