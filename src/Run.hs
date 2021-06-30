module Run
    ( run
    ) where

import Import
import qualified RIO.List as L
import qualified RIO.Seq as Seq

import qualified Brick as B
import qualified Graphics.Vty as V (defAttr)

import Taskell.UI.Text.Split (withWidth)

data Name =
    Name
    deriving (Ord, Eq)

textWidget' :: Text -> Int -> B.Widget Name
textWidget' txt width =
    case withWidth width txt of
        Right lns -> B.vBox $ B.txt <$> lns
        Left _ -> B.emptyWidget

textWidget :: Text -> B.Widget Name
textWidget txt =
    B.Widget B.Greedy B.Greedy $ do
        width <- (^. B.availWidthL) <$> B.getContext
        B.render $ textWidget' txt width

draw :: Seq Text -> [B.Widget Name]
draw s = [B.vBox . toList $ B.padBottom (B.Pad 1) . B.hLimit 25 . textWidget <$> s]

run :: RIO App ()
run = do
    let app =
            B.App
                { B.appDraw = draw
                , B.appChooseCursor = \_ c -> L.lastMaybe c
                , B.appHandleEvent = B.resizeOrQuit
                , B.appStartEvent = pure
                , B.appAttrMap = const $ B.attrMap V.defAttr []
                }
    let initialState =
            Seq.fromList
                [ "The first task on the list is to do this"
                , "The second task here goes a little something like this"
                , "I'm wrapping, I'm wrapping, I'm wrap-wrap-wrapping"
                , "Fishy fish fishfishyfishyfishyfishfishyfishfish astronaut"
                ]
    void . liftIO $ B.defaultMain app initialState
