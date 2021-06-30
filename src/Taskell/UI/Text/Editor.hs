{-# LANGUAGE TemplateHaskell #-}

module Taskell.UI.Text.Editor
    ( Editor
    , EditorE
    , Cursor(..)
    , cursor
    , rows
    , dump
    , create
    , insert
    , backspace
    , top
    , bottom
    , up
    , down
    , right
    , left
    , end
    , begin
    , endOfLine
    , startOfLine
    , getRelativePosition
    , setCursorFromRelativePosition
    ) where

import RIO
import qualified RIO.List as L
import qualified RIO.Seq as Seq
import qualified RIO.Text as T

import qualified Brick.Widgets.Core as B (textWidth)

import Lens.Micro.TH (makeLenses)

import qualified Taskell.UI.Text.Parser as P
import qualified Taskell.UI.Text.Split as S (Row, Rows, split)

import qualified Error

data Cursor =
    Cursor
        { _horizontal :: Int
        , _vertical :: Int
        }
    deriving (Eq, Show)

makeLenses ''Cursor

new :: Cursor
new = Cursor 0 0

setHorizontal :: Int -> Cursor -> Cursor
setHorizontal h = horizontal .~ h

stepHorizontal :: Int -> Cursor -> Cursor
stepHorizontal h = horizontal %~ (+ h)

setVertical :: Int -> Cursor -> Cursor
setVertical v = vertical .~ v

stepVertical :: Int -> Cursor -> Cursor
stepVertical v = vertical %~ (+ v)

setCursor :: (Int, Int) -> Cursor -> Cursor
setCursor (h, v) cursor = cursor & horizontal .~ h & vertical .~ v

data Editor =
    Editor
        { _cursor :: Cursor
        , _position :: Int
        , _width :: Int
        , _rows :: S.Rows
        , _lineBreaks :: [Int]
        }
    deriving (Eq)

type EditorE = Error.EitherError Editor

makeLenses ''Editor

create :: Int -> Text -> EditorE
create w text = do
    rws <- S.split w text
    let lnBreaks = findLineBreaks rws
    end $ Editor new 0 w rws lnBreaks

update :: (Editor -> Editor) -> Editor -> EditorE
update fn ed = pure $ fn ed

updateIf :: Bool -> (Editor -> Editor) -> Editor -> EditorE
updateIf tf fn ed = pure $ bool ed (fn ed) tf

lineCount :: Editor -> Int -> Int
lineCount ed v = fromMaybe 0 c
  where
    c = snd <$> Seq.lookup v (ed ^. rows)

currentLineCount :: Editor -> Int
currentLineCount ed = lineCount ed v
  where
    v = ed ^. cursor . vertical

rowCount :: Editor -> Int
rowCount ed = Seq.length rws
  where
    rws = ed ^. rows

prune :: Editor -> EditorE
prune ed = do
    let h = ed ^. cursor . horizontal
    let limit = currentLineCount ed
    updateIf (h > currentLineCount ed) (cursor %~ setHorizontal limit) ed

-- Brick gives -1 for new line, which is unhelpful in this scenario
textWidth :: Text -> Int
textWidth "\n" = 1
textWidth txt = B.textWidth txt

setPos :: Editor -> EditorE
setPos ed = do
    pos <- getRelativePosition ed
    update (position .~ pos) ed

-- cursor movement
left :: Editor -> EditorE
left ed = setPos =<< updateIf (h /= 0) (cursor %~ stepHorizontal (-1)) ed
  where
    h = ed ^. cursor . horizontal

right :: Editor -> EditorE
right ed = setPos =<< updateIf (h < limit) (cursor %~ stepHorizontal 1) ed
  where
    h = ed ^. cursor . horizontal
    limit = currentLineCount ed

up :: Editor -> EditorE
up ed = setPos =<< prune =<< updateIf (v /= 0) (cursor %~ stepVertical (-1)) ed
  where
    v = ed ^. cursor . vertical

down :: Editor -> EditorE
down ed = setPos =<< prune =<< updateIf (v < limit) (cursor %~ stepVertical 1) ed
  where
    v = ed ^. cursor . vertical
    limit = rowCount ed - 1

begin :: Editor -> EditorE
begin ed = setPos =<< update (cursor .~ new) ed

end :: Editor -> EditorE
end ed = endOfLine =<< bottom ed

endOfLine :: Editor -> EditorE
endOfLine ed = setPos =<< update (cursor %~ setHorizontal pos) ed
  where
    pos = currentLineCount ed

startOfLine :: Editor -> EditorE
startOfLine ed = setPos =<< update (cursor %~ setHorizontal 0) ed

top :: Editor -> EditorE
top ed = setPos =<< update (cursor %~ setVertical 0) ed

bottom :: Editor -> EditorE
bottom ed = setPos =<< update (cursor %~ setVertical v) ed
  where
    rws = rowCount ed
    v = bool 0 (rws - 1) (rws > 0)

-- editing
findLineBreaks :: S.Rows -> [Int]
findLineBreaks = Seq.findIndicesL (\(parts, _) -> maybe False (== P.LineBreak) (L.headMaybe parts))

setRows :: S.Rows -> Editor -> EditorE
setRows rws = update ((rows .~ rws) . (lineBreaks .~ findLineBreaks rws))

addLineBreakOffset :: [Int] -> Int -> S.Row -> Int
addLineBreakOffset lnBreaks idx (_, count) =
    if L.elem idx lnBreaks
        then count + 1
        else count

getRelativePosition :: Editor -> Error.EitherError Int
getRelativePosition ed = do
    let rws = ed ^. rows
    pure $
        if null rws
            then 0
            else do
                let Cursor h v = ed ^. cursor
                let before = Seq.take v rws
                let lnBreaks = ed ^. lineBreaks
                let adjustBy = length $ [0 .. v] `L.intersect` lnBreaks
                let beforeCounts = snd <$> before
                sum beforeCounts + h + adjustBy

setCursorFromRelativePosition :: Int -> Editor -> EditorE
setCursorFromRelativePosition 0 ed = update (cursor .~ new) ed
setCursorFromRelativePosition pos ed = do
    let rws = ed ^. rows
    let lnBreaks = ed ^. lineBreaks
    let lengths = Seq.scanl (+) 0 (addLineBreakOffset lnBreaks `Seq.mapWithIndex` rws)
    idx <-
        Error.mEither "Cursor: no rows are shorter than position" $ Seq.findIndexR (< pos) lengths
    pos' <- Error.mEither "Cursor: cannot find relevant length" $ (pos -) <$> Seq.lookup idx lengths
    let pos'' = bool pos' (pos' - 1) (L.elem idx lnBreaks)
    update (cursor %~ setCursor (pos'', idx)) ed

edit :: (Int -> (Text, Text) -> (Text, Int)) -> Editor -> EditorE
edit fn ed = do
    let pos = ed ^. position
    let parts = T.splitAt pos (dump ed)
    let (txt, newPos) = fn pos parts
    updatedTxt <- S.split (ed ^. width) txt
    setCursorFromRelativePosition newPos =<< update (position .~ newPos) =<< setRows updatedTxt ed

backspace' :: Int -> (Text, Text) -> (Text, Int)
backspace' pos (before, after) = (txt, pos')
  where
    removed = T.takeEnd 1 before
    txt = T.dropEnd 1 before <> after
    pos' = pos - textWidth removed

insert' :: Text -> Int -> (Text, Text) -> (Text, Int)
insert' added pos (before, after) = (txt, pos')
  where
    txt = before <> added <> after
    pos' = pos + textWidth added

backspace :: Editor -> EditorE
backspace = edit backspace'

insert :: Text -> Editor -> EditorE
insert txt = edit (insert' txt)

-- dump
dumpRow :: P.Part -> Text
dumpRow (P.Word text) = text
dumpRow (P.Whitespace text) = text
dumpRow P.LineBreak = "\n"

dumpRowToText :: P.Parts -> Text
dumpRowToText row = T.concat $ dumpRow <$> row

dumpRows :: S.Rows -> Text
dumpRows rws = T.concat . toList $ dumpRowToText . fst <$> rws

dump :: Editor -> Text
dump ed = dumpRows (ed ^. rows)
