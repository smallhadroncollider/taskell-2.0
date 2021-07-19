module Taskell.IO.MarkDown.Parser.DocumentSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.IO.MarkDown.Parser.Document (parse)
import Taskell.IO.MarkDown.Parser.Types
import Taskell.IO.MarkDown.Types

file :: IO Text
file = readFileUtf8 "test/Taskell/IO/MarkDown/Parser/document.md"

-- tests
spec :: Spec
spec = do
    describe "title" $ do
        it "parses the title" $ do
            input <- liftIO file
            (^. taskellTitle) <$> parse defaultDictionary input `shouldBe` Right "Blah"
    describe "description" $ do
        it "parses the description" $ do
            input <- liftIO file
            (^. taskellDescription) <$>
                parse defaultDictionary input `shouldBe`
                Right "A bunch of lists in a thing.\n\nSee how many lists there are!"
    describe "contributors" $ do
        it "parses the contributors" $ do
            input <- liftIO file
            (^. taskellContributors) <$>
                parse defaultDictionary input `shouldBe`
                Right
                    [ ParsedContributor "Mark" "Mark Wales" "mark@smallhadroncollider.com"
                    , ParsedContributor "Jess" "Jess Munroe" "jess@munroesoft.com"
                    ]
    describe "lists" $ do
        it "parses the list titles" $ do
            input <- liftIO file
            ((^. listTitle) <$>) . (^. taskellLists) <$>
                parse defaultDictionary input `shouldBe` Right ["Things", "Stuff", "More Stuff"]
    describe "tasks" $ do
        it "parses the tasks" $ do
            input <- liftIO file
            (((^. taskTitle) <$>) . (^. listTasks) <$>) . (^. taskellLists) <$>
                parse defaultDictionary input `shouldBe`
                Right
                    [ ["Hello Mum", "How are you?", "This is a task"]
                    , ["Do a thing", "Eat a thing", "Poke a thing"]
                    , ["Spoons for all!!", "Forks for all!!", "Sporks for no one"]
                    ]
