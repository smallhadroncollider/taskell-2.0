module Taskell.IO.MarkDown.Convert.ToSerializedSpec
    ( spec
    ) where

import RIO
import qualified RIO.List as L

import Test.Hspec

import qualified Taskell.Error as Error

import Taskell.Data.Types.Task (TaskID(..))
import qualified Taskell.IO.MarkDown.Convert.FromSerialized as F (convert)
import Taskell.IO.MarkDown.Convert.ToSerialized (convert, relatedDictionary)
import Taskell.IO.MarkDown.Types

smoosh :: Error.EitherError (Maybe a) -> Error.EitherError a
smoosh (Left a) = Left a
smoosh (Right Nothing) = Error.e "Nothing"
smoosh (Right (Just a)) = Right a

-- tests
spec :: Spec
spec =
    parallel $ do
        tmpData <-
            runIO $
            F.convert defaultDictionary <$> readFileUtf8 "test/Taskell/IO/MarkDown/output.md"
        describe "related dictionary" $ do
            it "generates dictionary" $ do
                (relatedDictionary =<< tmpData) `shouldBe`
                    Right
                        [ (TaskID 1, ("First List", "First Task", "first-task"))
                        , (TaskID 5, ("First List", "Third Task", "third-task"))
                        , (TaskID 6, ("First List", "Fifth Task", "fifth-task"))
                        , (TaskID 7, ("Second List", "Second Task", "second-task"))
                        , (TaskID 8, ("Second List", "Fourth Task", "fourth-task"))
                        ]
        describe "convert" $ do
            describe "title" $ do
                it "gets the title" $ do
                    (^. taskellTitle) <$> (convert =<< tmpData) `shouldBe` Right "Test"
            describe "description" $ do
                it "gets the description" $ do
                    (^. taskellDescription) <$>
                        (convert =<< tmpData) `shouldBe` Right "Some test data"
            describe "contributors" $ do
                it "sorts the contributors alphabetically by name" $ do
                    (^. taskellContributors) <$>
                        (convert =<< tmpData) `shouldBe`
                        Right
                            [ SerializedContributor "Bob" "Bob" "bob@bob.com"
                            , SerializedContributor "Jenny" "Jenny" "jenny@jenny.com"
                            , SerializedContributor "Jim" "Jim" "jim@jim.com"
                            ]
            describe "lists" $ do
                let lists = (^. taskellLists) <$> (convert =<< tmpData)
                let firstList = smoosh $ L.headMaybe <$> lists
                it "sorts lists correctly" $ do
                    (^. listTitle) <$> firstList `shouldBe` Right "First List"
                it "adds correct number of tasks" $ do
                    length . (^. listTasks) <$> firstList `shouldBe` Right 3
            describe "tasks" $ do
                let lists = (^. taskellLists) <$> (convert =<< tmpData)
                let firstList = smoosh $ L.headMaybe <$> lists
                let firstTask = smoosh $ L.headMaybe . (^. listTasks) <$> firstList
                it "has correct title" $ do
                    (^. taskTitle) <$> firstTask `shouldBe` Right "First Task"
                it "has correct description" $ do
                    (^. taskDescription) <$> firstTask `shouldBe` Right (Just "Do first thing")
                it "has correct completion status" $ do
                    (^. taskComplete) <$> firstTask `shouldBe` Right False
                it "has contributors" $ do
                    (^. taskContributors) <$> firstTask `shouldBe` Right ["Bob", "Jenny"]
                it "has tags" $ do (^. taskTags) <$> firstTask `shouldBe` Right ["fish", "cow"]
                it "has related tasks" $ do
                    (^. taskRelated) <$>
                        firstTask `shouldBe`
                        Right
                            [ ("Second List", "Second Task", "second-task")
                            , ("First List", "Third Task", "third-task")
                            , ("First List", "Fifth Task", "fifth-task")
                            ]
                it "has sub-tasks" $ do
                    (^. taskTasks) <$>
                        firstTask `shouldBe`
                        Right
                            [ SerializedTask
                                  "Sub Task"
                                  (Just "Sub task")
                                  False
                                  [ SerializedTask
                                        "Sub Sub Task"
                                        Nothing
                                        True
                                        [ SerializedTask
                                              "Sub Sub Sub Task"
                                              (Just "Sub sub sub task")
                                              False
                                              []
                                              []
                                              []
                                              []
                                        ]
                                        ["spatula"]
                                        []
                                        []
                                  ]
                                  []
                                  []
                                  []
                            ]
