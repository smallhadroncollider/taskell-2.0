{-# LANGUAGE TemplateHaskell #-}

module UI.Text.Editor where

import RIO
import qualified RIO.List as L
import qualified RIO.Seq as Seq
import qualified RIO.Text as T

import qualified Brick.Widgets.Core as B (textWidth)

import Lens.Micro.TH (makeLenses)

import qualified UI.Text.Parser as P
import qualified UI.Text.Split as S (Rows, split)

import qualified Error

data Cursor =
    Cursor
        { _horizontal :: Int
        , _vertical :: Int
        }
    deriving (Eq)

makeLenses ''Cursor

new :: Cursor
new = Cursor 0 0

setHorizontal :: Int -> Cursor -> Cursor
setHorizontal h =
    horizontal .~
    if h > 0
        then h
        else 0

stepHorizontal :: Int -> Cursor -> Cursor
stepHorizontal h cursor = setHorizontal (cursor ^. horizontal + h) cursor

setVertical :: Int -> Cursor -> Cursor
setVertical v =
    vertical .~
    if v > 0
        then v
        else 0

stepVertical :: Int -> Cursor -> Cursor
stepVertical v cursor = setVertical (cursor ^. vertical + v) cursor

setCursor :: (Int, Int) -> Cursor -> Cursor
setCursor (h, v) cursor = setHorizontal h $ setVertical v cursor

data Editor =
    Editor
        { _cursor :: Cursor
        , _position :: Int
        , _width :: Int
        , _rows :: S.Rows
        }
    deriving (Eq)

type EditorE = Error.EitherError Editor

makeLenses ''Editor

create :: Int -> Text -> EditorE
create w text = do
    rws <- S.split w text
    end $ Editor new 0 w rws

update :: (Editor -> Editor) -> Editor -> EditorE
update fn ed = pure $ fn ed

updateIf :: Bool -> (Editor -> Editor) -> Editor -> EditorE
updateIf tf fn ed = pure $ bool ed (fn ed) tf

lineCount :: Editor -> Int -> Int
lineCount ed v = maybe 0 count mLine
  where
    mLine = Seq.lookup v (ed ^. rows)

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

countPart :: P.Part -> Int
countPart (P.Word text) = B.textWidth text
countPart (P.Whitespace text) = B.textWidth text
countPart P.LineBreak = 1

count :: [P.Part] -> Int
count parts = sum $ countPart <$> parts

setPos :: Editor -> EditorE
setPos ed = update (position .~ getRelativePosition ed) ed

setCur :: Editor -> EditorE
setCur ed = do
    let pos = ed ^. position
    cur <- getCursorFromRelativePosition pos ed
    update (cursor .~ cur) ed

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
    limit = length (ed ^. rows) - 1

begin :: Editor -> EditorE
begin ed = setPos =<< update (cursor .~ new) ed

end :: Editor -> EditorE
end ed = bottom ed >>= endOfLine

endOfLine :: Editor -> EditorE
endOfLine ed = setPos =<< update (cursor %~ setHorizontal pos) ed
  where
    pos = currentLineCount ed

startOfLine :: Editor -> EditorE
startOfLine ed = setPos =<< update (cursor %~ setHorizontal 0) ed

top :: Editor -> EditorE
top ed = setPos =<< update (cursor %~ setVertical 0) ed

bottom :: Editor -> EditorE
bottom ed = setPos =<< update (cursor %~ setVertical (rowCount ed - 1)) ed

-- editing
getRelativePosition :: Editor -> Int
getRelativePosition ed = beforeCount + h
  where
    Cursor h v = ed ^. cursor
    rws = ed ^. rows
    before = Seq.take v rws
    beforeCount = sum $ count <$> before

getCursorFromRelativePosition :: Int -> Editor -> Error.EitherError Cursor
getCursorFromRelativePosition 0 _ = pure new
getCursorFromRelativePosition pos ed = do
    let rws = ed ^. rows
    -- get lengths of all rows
    let lengths = Seq.scanl (+) 0 (count <$> rws)
    -- find index of last row that's under length
    idx <-
        Error.mEither "Cursor: no rows are shorter than position" $ Seq.findIndexR (< pos) lengths
    -- vertical will be next row
    let v = idx
    pos' <- Error.mEither "Cursor: cannot find relevant length" $ (pos -) <$> Seq.lookup idx lengths
    line <- Error.mEither "Cursor: cannot find relevant row" $ Seq.lookup idx rws
    f <- Error.mEither "Cursor: empty line" $ L.headMaybe line
    let pos'' =
            if f == P.LineBreak
                then pos' - 1
                else pos'
    pure $ Cursor pos'' v

backspace :: Editor -> EditorE
backspace ed = do
    let pos = ed ^. position
    let txt = dump ed
    let (before, after) = T.splitAt pos txt
    let removed = T.takeEnd 1 before
    let removedLength =
            if removed == "\n"
                then 1
                else B.textWidth removed
    updatedTxt <- S.split (ed ^. width) (T.dropEnd 1 before <> after)
    let pos' = pos - removedLength
    ed' <- update ((rows .~ updatedTxt) . (position .~ pos')) ed
    newCursor <- getCursorFromRelativePosition pos' ed'
    update (cursor .~ newCursor) ed'

insert :: Char -> Editor -> EditorE
insert char ed = do
    let pos = ed ^. position
    let txt = dump ed
    let (before, after) = T.splitAt pos txt
    let extra = T.singleton char
    updatedTxt <- S.split (ed ^. width) (before <> extra <> after)
    let pos' =
            if char == '\n'
                then pos + 1
                else pos + B.textWidth extra
    ed' <- update ((rows .~ updatedTxt) . (position .~ pos')) ed
    newCursor <- getCursorFromRelativePosition pos' ed'
    update (cursor .~ newCursor) ed'

-- dump
dumpRow :: P.Part -> Text
dumpRow (P.Word text) = text
dumpRow (P.Whitespace text) = text
dumpRow P.LineBreak = "\n"

dumpRowToText :: P.Parts -> Text
dumpRowToText row = T.concat $ dumpRow <$> row

dumpRows :: S.Rows -> Text
dumpRows rws = T.concat . toList $ dumpRowToText <$> rws

dump :: Editor -> Text
dump ed = dumpRows (ed ^. rows)
