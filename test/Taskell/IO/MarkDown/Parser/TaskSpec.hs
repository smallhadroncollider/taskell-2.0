module Taskell.IO.MarkDown.Parser.TaskSpec
    ( spec
    ) where

import RIO
import qualified RIO.List as L

import Test.Hspec

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Task
import Taskell.IO.MarkDown.Types

file :: IO Text
file = readFileUtf8 "test/Taskell/IO/MarkDown/Parser/task.md"

parse :: IO (Either String SerializedTask)
parse = P.parseOnly (taskP defaultDictionary 0) <$> file

-- tests
spec :: Spec
spec = do
    output <- runIO parse
    describe "title" $ do
        it "parses the title" $ do (^. taskTitle) <$> output `shouldBe` Right "Hello Mum"
    -- describe "due" $ do
    --     it "parses the due date" $ do
    --         output <- liftIO parse
    --         (^. taskDue) <$> output `shouldBe` Right (Just "2021-12-03 12:00")
    describe "description" $ do
        it "parses the description" $ do
            (^. taskDescription) <$>
                output `shouldBe` Right (Just "This is a thing with some stuff\n\nBlah blah blah")
    describe "tasks" $ do
        let tasks = (^. taskTasks) <$> output
        it "parses the tasks" $ do
            ((^. taskTitle) <$>) <$>
                tasks `shouldBe` Right ["A sub-task with the thing", "Nonsense"]
        it "parses the completion" $ do
            ((^. taskComplete) <$>) <$> tasks `shouldBe` Right [True, False]
        it "parses the descriptions" $ do
            ((^. taskDescription) <$>) <$> tasks `shouldBe` Right [Just "Sub task", Nothing]
        it "parses the tags" $ do
            ((^. taskTags) <$>) <$> tasks `shouldBe` Right [["submarine", "subcutaneous"], []]
        describe "sub-tasks" $ do
            let sub = L.headMaybe <$> tasks
            it "has sub tasks" $ do
                ((^. taskTasks) <$>) <$>
                    sub `shouldBe`
                    Right (Just [SerializedTask "Sub Sub Task" Nothing True [] [] [] []])
    describe "tags" $ do
        it "parses the tags" $ do (^. taskTags) <$> output `shouldBe` Right ["thing", "things"]
    describe "related" $ do
        it "parses the related tasks" $ do
            (^. taskRelated) <$>
                output `shouldBe`
                Right
                    [ ("Stuff", "Hello Mum", "hello-mum-1")
                    , ("Things", "How are you?", "how-are-you")
                    ]
    describe "contributors" $ do
        it "parses the contributors" $ do
            (^. taskContributors) <$> output `shouldBe` Right ["Mark", "Jess"]
