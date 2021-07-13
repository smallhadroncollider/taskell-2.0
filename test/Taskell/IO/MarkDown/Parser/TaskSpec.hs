module Taskell.IO.MarkDown.Parser.TaskSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Task
import Taskell.IO.MarkDown.Parser.Types

file :: IO Text
file = readFileUtf8 "test/Taskell/IO/MarkDown/Parser/task.md"

parse :: IO (Either String AlmostTask)
parse = P.parseOnly (taskP defaultDictionary) <$> file

-- tests
spec :: Spec
spec = do
    describe "title" $ do
        it "parses the title" $ do
            output <- liftIO parse
            (^. taskTitle) <$> output `shouldBe` Right "Hello Mum"
    describe "due" $ do
        it "parses the due date" $ do
            output <- liftIO parse
            (^. taskDue) <$> output `shouldBe` Right "2021-12-03 12:00"
    describe "description" $ do
        it "parses the description" $ do
            output <- liftIO parse
            (^. taskDescription) <$>
                output `shouldBe` Right "This is a thing with some stuff\n\nBlah blah blah"
    describe "tasks" $ do
        it "parses the tasks" $ do
            output <- liftIO parse
            ((^. taskTitle) <$>) . (^. taskTasks) <$>
                output `shouldBe` Right ["A sub-task with the thing", "Nonsense"]
        it "parses the task - complete" $ do
            output <- liftIO parse
            ((^. taskComplete) <$>) . (^. taskTasks) <$> output `shouldBe` Right [True, False]
