module Taskell.IO.MarkDown.Convert.GitHubLinksSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.Data.Types.Task (TaskID(..))
import Taskell.IO.MarkDown.Convert.GitHubLinks (generateLinks)

-- tests
spec :: Spec
spec = do
    describe "sequence links" $ do
        it "sequences links" $ do
            generateLinks
                [ (TaskID 1, "A header")
                , (TaskID 3, "Another header")
                , (TaskID 5, "A Header")
                , (TaskID 7, "Yet Another | Header")
                , (TaskID 9, "Head     |     er")
                , (TaskID 2, "             Space!")
                , (TaskID 4, "The 1, 2, 3, 4!")
                , (TaskID 6, "H&D")
                , (TaskID 8, "The 🙀 apocalypse")
                , (TaskID 10, "H-D")
                , (TaskID 11, "Sponges_are_us")
                , (TaskID 12, "a header")
                , (TaskID 13, "a header              ")
                ] `shouldBe`
                [ (TaskID 1, "a-header")
                , (TaskID 3, "another-header")
                , (TaskID 5, "a-header-1")
                , (TaskID 7, "yet-another--header")
                , (TaskID 9, "head----------er")
                , (TaskID 2, "space")
                , (TaskID 4, "the-1-2-3-4")
                , (TaskID 6, "hd")
                , (TaskID 8, "the--apocalypse")
                , (TaskID 10, "h-d")
                , (TaskID 11, "sponges_are_us")
                , (TaskID 12, "a-header-2")
                , (TaskID 13, "a-header-3")
                ]
