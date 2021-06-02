{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
    ( run
    ) where

import Import
import qualified RIO.List as L

import qualified Brick as B
import qualified Graphics.Vty as V (defAttr)

import UI.Text.Split (withWidth)

data Name =
    Name
    deriving (Ord, Eq)

textWidget :: Text -> B.Widget Name
textWidget txt =
    case withWidth 20 txt of
        Left _ -> B.emptyWidget
        Right lns -> B.vBox $ B.txt <$> lns

draw :: [Text] -> [B.Widget Name]
draw s = [B.vBox $ B.padBottom (B.Pad 1) . textWidget <$> s]

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
            [ "The first task on the list is to do this"
            , "The second task here goes a little something like this"
            , "I'm wrapping, I'm wrapping, I'm wrap-wrap-wrapping"
            ]
    void . liftIO $ B.defaultMain app initialState
