module RunEditor
    ( run
    ) where

import Import

import qualified RIO.List as L
import qualified RIO.Text as T

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import qualified Graphics.Vty as V

import Taskell.Error as Error
import qualified Taskell.UI.Text.Editor as E
import Taskell.UI.Text.Split (rowsToText)

data Name =
    Name
    deriving (Ord, Eq)

emptyLine :: Text -> Text
emptyLine t = bool t " " (t == "")

edWidget :: E.Editor -> B.Widget Name
edWidget ed = w B.<=> c
  where
    (E.Cursor x y) = ed ^. E.cursor
    rows = ed ^. E.rows
    w = B.showCursor Name (B.Location (x, y)) . B.vBox $ B.txt . emptyLine <$> rowsToText rows
    c = B.padTop (B.Pad 1) (B.txt ("(" <> tshow x <> "," <> tshow y <> ")"))

draw :: E.EditorE -> [B.Widget Name]
draw editorE =
    case editorE of
        Right ed -> [B.center $ edWidget ed]
        Left (Error.Error err) -> [B.txt err]

appHandleEvent :: E.EditorE -> B.BrickEvent n e -> B.EventM n (B.Next E.EditorE)
appHandleEvent ed (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt ed
appHandleEvent ed (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (E.up =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (E.down =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey V.KLeft [])) = B.continue (E.left =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey V.KRight [])) = B.continue (E.right =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey V.KBS [])) = B.continue (E.backspace =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey V.KEnter [])) = B.continue (E.insert "\n" =<< ed)
appHandleEvent _ (B.VtyEvent (V.EvKey (V.KChar '§') [])) = B.continue (E.create 50 "")
appHandleEvent ed (B.VtyEvent (V.EvKey (V.KChar '∆') [])) = B.continue (E.top =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey (V.KChar '˚') [])) = B.continue (E.bottom =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey (V.KChar '˙') [])) = B.continue (E.begin =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey (V.KChar '¬') [])) = B.continue (E.end =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvKey (V.KChar c) [])) =
    B.continue (E.insert (T.singleton c) =<< ed)
appHandleEvent ed (B.VtyEvent (V.EvPaste bs)) =
    B.continue (E.insert (T.decodeUtf8With T.lenientDecode bs) =<< ed)
appHandleEvent ed _ = B.continue ed

appStart :: E.EditorE -> B.EventM Name E.EditorE
appStart state = do
    output <- V.outputIface <$> B.getVtyHandle
    when (V.supportsMode output V.BracketedPaste) . liftIO $ V.setMode output V.BracketedPaste True
    pure state

run :: RIO App ()
run = do
    let app =
            B.App
                { B.appDraw = draw
                , B.appChooseCursor = \_ c -> L.lastMaybe c
                , B.appHandleEvent = appHandleEvent
                , B.appStartEvent = appStart
                , B.appAttrMap = const $ B.attrMap V.defAttr []
                }
    let initialState = E.create 50 ""
    void . liftIO $ B.defaultMain app initialState
