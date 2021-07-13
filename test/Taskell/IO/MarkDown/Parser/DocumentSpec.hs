module Taskell.IO.MarkDown.Parser.DocumentSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Data.Types.Taskell as Taskell

import Taskell.IO.MarkDown.Parser.Document (parse)
import Taskell.IO.MarkDown.Parser.Types (defaultDictionary)

file :: IO Text
file = readFileUtf8 "test/Taskell/IO/MarkDown/Parser/document.md"

-- tests
spec :: Spec
spec = do
    describe "title" $ do
        it "parses the title" $ do
            input <- liftIO file
            (^. Taskell.title) <$> parse input defaultDictionary `shouldBe` Right "Blah"
    describe "description" $ do
        it "parses the description" $ do
            input <- liftIO file
            (^. Taskell.description) <$>
                parse input defaultDictionary `shouldBe`
                Right "A bunch of lists in a thing.\n\nSee how many lists there are!"
    describe "contributors" $ do
        it "parses the contributors" $ do
            input <- liftIO file
            (^. Taskell.contributors) <$>
                parse input defaultDictionary `shouldBe`
                Right
                    [ ( Contributor.ContributorID 1
                      , Contributor.Contributor "Mark" "Mark Wales" "mark@smallhadroncollider.com")
                    , ( Contributor.ContributorID 2
                      , Contributor.Contributor "Jess" "Jess Munroe" "jess@munroesoft.com")
                    ]
    describe "list order" $ do
        it "parses the list order" $ do
            input <- liftIO file
            (^. Taskell.listsOrder) <$>
                parse input defaultDictionary `shouldBe`
                Right [List.ListID 1, List.ListID 2, List.ListID 3]
    describe "lists" $ do
        it "parses the list titles" $ do
            input <- liftIO file
            ((^. List.title) <$>) . (^. Taskell.lists) <$>
                parse input defaultDictionary `shouldBe`
                Right
                    [ (List.ListID 1, "Things")
                    , (List.ListID 2, "Stuff")
                    , (List.ListID 3, "More Stuff")
                    ]
    describe "tasks" $ do
        it "parses the tasks" $ do
            input <- liftIO file
            ((^. Task.title) <$>) . (^. Taskell.tasks) <$>
                parse input defaultDictionary `shouldBe`
                Right
                    [ (Task.TaskID 1, "Hello Mum")
                    , (Task.TaskID 2, "How are you?")
                    , (Task.TaskID 3, "This is a task")
                    , (Task.TaskID 4, "Do a thing")
                    , (Task.TaskID 5, "Eat a thing")
                    , (Task.TaskID 6, "Poke a thing")
                    , (Task.TaskID 7, "Spoons for all!!")
                    , (Task.TaskID 8, "Forks for all!!")
                    , (Task.TaskID 9, "Sporks for no one")
                    ]
    describe "contributors" $ do
        it "parses the task contributors" $ do
            input <- liftIO file
            ((^. Task.assigned) <$>) . (^. Taskell.tasks) <$>
                parse input defaultDictionary `shouldBe`
                Right
                    [ (Task.TaskID 1, [Contributor.ContributorID 1, Contributor.ContributorID 2])
                    , (Task.TaskID 2, [Contributor.ContributorID 1])
                    , (Task.TaskID 3, [Contributor.ContributorID 2])
                    , (Task.TaskID 4, [])
                    , (Task.TaskID 5, [])
                    , (Task.TaskID 6, [])
                    , (Task.TaskID 7, [Contributor.ContributorID 1, Contributor.ContributorID 2])
                    , (Task.TaskID 8, [])
                    , (Task.TaskID 9, [])
                    ]
