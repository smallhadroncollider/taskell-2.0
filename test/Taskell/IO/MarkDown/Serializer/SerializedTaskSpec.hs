{-# LANGUAGE QuasiQuotes #-}

module Taskell.IO.MarkDown.Serializer.SerializedTaskSpec
    ( spec
    ) where

import RIO

import NeatInterpolation (text)

import Test.Hspec

import Taskell.IO.MarkDown.Serializer.Serialize (taskS)
import Taskell.IO.MarkDown.Types

-- was outputting too many newlines before tags
check1 :: SerializedTask
check1 =
    SerializedTask
        { _taskTitle = "B"
        , _taskDescription = Nothing
        , _taskComplete = True
        , _taskTasks =
              [ SerializedTask
                    { _taskTitle = "C"
                    , _taskDescription = Nothing
                    , _taskComplete = True
                    , _taskTasks = []
                    , _taskTags = []
                    , _taskRelated = []
                    , _taskContributors = []
                    }
              ]
        , _taskTags = ["x"]
        , _taskRelated = []
        , _taskContributors = []
        }

output1 :: Text
output1 =
    [text|

        - [x] B

            - [x] C

            `#x`

    |]

-- not outputting newlines before next sub-task
check2 :: SerializedTask
check2 =
    SerializedTask
        { _taskTitle = "A"
        , _taskDescription = Nothing
        , _taskComplete = False
        , _taskTasks =
              [ SerializedTask
                    { _taskTitle = "B"
                    , _taskDescription = Nothing
                    , _taskComplete = True
                    , _taskTasks = []
                    , _taskTags = ["x"]
                    , _taskRelated = []
                    , _taskContributors = []
                    }
              , SerializedTask
                    { _taskTitle = "D"
                    , _taskDescription = Nothing
                    , _taskComplete = False
                    , _taskTasks = []
                    , _taskTags = []
                    , _taskRelated = []
                    , _taskContributors = []
                    }
              ]
        , _taskTags = []
        , _taskRelated = []
        , _taskContributors = []
        }

output2 :: Text
output2 =
    [text|

        ### A

        - [x] B

            `#x`

        - [ ] D

    |]

-- tests
spec :: Spec
spec =
    parallel $ do
        describe "serializes tasks correctly" $ do
            it "sub task newlines - level 2+" $ do
                utf8BuilderToText (runReader (taskS 1 check1) defaultDictionary) `shouldBe` output1
            it "sub task newlines - level 1" $ do
                utf8BuilderToText (runReader (taskS 0 check2) defaultDictionary) `shouldBe` output2
