module Taskell.UI.Text.EditorUseSpec
    ( spec
    ) where

import RIO
import qualified RIO.List as L

import Test.Hspec

import Taskell.UI.Text.Editor

start :: EditorE
start = create 20 ""

operations :: [Editor -> EditorE]
operations =
    [ insert "h"
    , insert "e"
    , insert "l"
    , insert "l"
    , insert "o"
    , insert " "
    , insert "\n"
    , insert "\n"
    , insert "h"
    , insert "o"
    , insert "w"
    , insert " "
    , insert "a"
    , insert "r"
    , insert "e"
    , insert " "
    , backspace
    ]

--    0 1 2 3 4 5 6 7
-- 0  h e l l o _
-- 1
-- 2  h o w _ a r e _
expected :: [(Cursor, Text)]
expected =
    [ (Cursor 0 0, "")
    , (Cursor 1 0, "h")
    , (Cursor 2 0, "he")
    , (Cursor 3 0, "hel")
    , (Cursor 4 0, "hell")
    , (Cursor 5 0, "hello")
    , (Cursor 6 0, "hello ")
    , (Cursor 0 1, "hello \n")
    , (Cursor 0 2, "hello \n\n")
    , (Cursor 1 2, "hello \n\nh")
    , (Cursor 2 2, "hello \n\nho")
    , (Cursor 3 2, "hello \n\nhow")
    , (Cursor 4 2, "hello \n\nhow ")
    , (Cursor 5 2, "hello \n\nhow a")
    , (Cursor 6 2, "hello \n\nhow ar")
    , (Cursor 7 2, "hello \n\nhow are")
    , (Cursor 8 2, "hello \n\nhow are ")
    , (Cursor 7 2, "hello \n\nhow are")
    ]

-- tests
spec :: Spec
spec =
    parallel $ do
        describe "cursor position" $ do
            let editors = L.scanl' (>>=) start operations
            let cursors = ((\e -> (e ^. cursor, dump e)) <$>) <$> editors
            it "cursors" $ cursors `shouldBe` (Right <$> expected)
