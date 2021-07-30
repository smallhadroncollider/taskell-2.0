module Run
    ( run
    ) where

import Import
import qualified RIO.List as L
import qualified RIO.Seq as Seq

import qualified Brick as B
import qualified Graphics.Vty as V (defAttr)

import qualified Taskell.Data.Taskell as T (ListTuple, Taskell, getListsWithIDs, tasksForList)
import qualified Taskell.Data.Types.List as TL (title)
import qualified Taskell.Data.Types.Task as TT (Task, title)
import qualified Taskell.Error as Error
import Taskell.UI.State (State(..), StateReader, taskell)
import Taskell.UI.Text.Split (withWidth)

import TmpData (tmpData)

data Name =
    Name
    deriving (Ord, Eq)

get :: StateReader T.Taskell
get = (^. taskell) <$> ask

err :: (b -> StateReader (B.Widget Name)) -> Error.EitherError b -> StateReader (B.Widget Name)
err _ (Left (Error.Error e)) = pure $ textWidget e
err fn (Right b) = fn b

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

taskWidget :: TT.Task -> B.Widget Name
taskWidget task = textWidget (task ^. TT.title)

taskWidgets :: Seq TT.Task -> StateReader (B.Widget Name)
taskWidgets tasks = do
    let items = toList (taskWidget <$> tasks)
    pure $ B.vBox $ B.padTop (B.Pad 1) <$> items

listWidget :: Int -> T.ListTuple -> StateReader (B.Widget Name)
listWidget index (listID, list) = do
    tskl <- get
    tasks <- err taskWidgets (T.tasksForList listID tskl)
    let title = tshow (index + 1) <> ". " <> list ^. TL.title
    pure $ textWidget title B.<=> tasks

listWidgets :: Seq T.ListTuple -> StateReader (B.Widget Name)
listWidgets lists = do
    listWs <- sequence (listWidget `Seq.mapWithIndex` lists)
    let sized = B.padTop (B.Pad 1) . B.padLeftRight 3 . B.hLimit 25 <$> listWs
    pure . B.hBox $ toList sized

drawS :: StateReader [B.Widget Name]
drawS = do
    tskl <- get
    widget <- err listWidgets (T.getListsWithIDs tskl)
    pure [widget]

draw :: State -> [B.Widget Name]
draw = runReader drawS

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
    let initialState = State tmpData
    void . liftIO $ B.defaultMain app initialState
