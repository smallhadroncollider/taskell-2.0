module Taskell.IO.MarkDown.Parser.DocumentSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.IO.MarkDown.Parser.Document (listP, parse)
import Taskell.IO.MarkDown.Types
import qualified Taskell.Utility.Parser as P

-- tests
spec :: Spec
spec =
    parallel $ do
        describe "normal file" $ do
            input <- runIO $ readFileUtf8 "test/Taskell/IO/MarkDown/Parser/document.md"
            describe "title" $ do
                it "parses the title" $ do
                    (^. taskellTitle) <$> parse defaultDictionary input `shouldBe` Right "Blah"
            describe "description" $ do
                it "parses the description" $ do
                    (^. taskellDescription) <$>
                        parse defaultDictionary input `shouldBe`
                        Right "A bunch of lists in a thing.\n\nSee how many lists there are!"
            describe "contributors" $ do
                it "parses the contributors" $ do
                    (^. taskellContributors) <$>
                        parse defaultDictionary input `shouldBe`
                        Right
                            [ SerializedContributor "Jess" "Jess Munroe" "jess@munroesoft.com"
                            , SerializedContributor
                                  "Mark"
                                  "Mark Wales"
                                  "mark@smallhadroncollider.com"
                            ]
            describe "lists" $ do
                it "parses the list titles" $ do
                    ((^. listTitle) <$>) . (^. taskellLists) <$>
                        parse defaultDictionary input `shouldBe`
                        Right ["Things", "Stuff", "More Stuff"]
            describe "tasks" $ do
                it "parses the tasks" $ do
                    (((^. taskTitle) <$>) . (^. listTasks) <$>) . (^. taskellLists) <$>
                        parse defaultDictionary input `shouldBe`
                        Right
                            [ ["Hello Mum", "How are you?", "This is a task"]
                            , ["Do a thing", "Eat a thing", "This is a task"]
                            , ["Spoons for all!!", "Forks for all!!", "Sporks for no one"]
                            ]
        describe "dodgy file" $ do
            input <- runIO $ readFileUtf8 "test/Taskell/IO/MarkDown/Parser/dodgy.md"
            describe "title" $ do
                it "parses the title" $ do
                    (^. listTitle) <$>
                        P.parseOnly (listP defaultDictionary <* P.endOfInput) input `shouldBe`
                        Right "B C"
