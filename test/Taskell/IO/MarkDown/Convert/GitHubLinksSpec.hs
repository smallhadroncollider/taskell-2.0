module Taskell.IO.MarkDown.Convert.GitHubLinksSpec
    ( spec
    ) where

import RIO

import Test.Hspec

import Taskell.IO.MarkDown.Convert.GitHubLinks (generateLinks)

-- tests
spec :: Spec
spec =
    parallel $ do
        describe "sequence links" $ do
            it "sequences links" $ do
                generateLinks
                    [ "A header"
                    , "Another header"
                    , "A Header"
                    , "Yet Another | Header"
                    , "Head     |     er"
                    , "             Space!"
                    , "The 1, 2, 3, 4!"
                    , "H&D"
                    , "The 🙀 apocalypse"
                    , "H-D"
                    , "Sponges_are_us"
                    , "a header"
                    , "a header              "
                    ] `shouldBe`
                    [ "a-header"
                    , "another-header"
                    , "a-header-1"
                    , "yet-another--header"
                    , "head----------er"
                    , "space"
                    , "the-1-2-3-4"
                    , "hd"
                    , "the--apocalypse"
                    , "h-d"
                    , "sponges_are_us"
                    , "a-header-2"
                    , "a-header-3"
                    ]
