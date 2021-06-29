module UI.Text.Split
    ( Rows
    , Parts
    , withWidth
    , split
    , rowsToText
    ) where

import RIO
import qualified RIO.Seq as Seq
import qualified RIO.Text as T (concat, splitAt)

import qualified Brick.Widgets.Core as B (textWidth)

import UI.Text.Parser

import qualified Error

type ReaderWidth = Reader Int

type Rows = Seq.Seq Parts

type Accumulator = (Maybe (Parts, Int), Rows)

splitLongWord :: Accumulator -> Text -> ReaderWidth Accumulator
splitLongWord (acc, rest) text = do
    width <- ask
    let (firstLine, secondLine) = T.splitAt width text
    let rest' = maybe rest ((rest Seq.|>) . fst) acc
    let rest'' = rest' <> Seq.singleton [Word firstLine]
    if B.textWidth secondLine > width
        then splitLongWord (Nothing, rest'') secondLine
        else pure (Just ([Word secondLine], B.textWidth secondLine), rest'')

joinWord :: Accumulator -> Text -> ReaderWidth Accumulator
joinWord (Nothing, rest) text = pure (Just ([Word text], B.textWidth text), rest)
joinWord (Just (parts, lng), rest) text = do
    let tLng = B.textWidth text
    width <- ask
    pure $
        if lng + tLng > width
            then (Just ([Word text], tLng), rest Seq.|> parts)
            else (Just (parts <> [Word text], lng + tLng), rest)

splitWord :: Accumulator -> Text -> ReaderWidth Accumulator
splitWord acc text = do
    width <- ask
    if B.textWidth text > width
        then splitLongWord acc text
        else joinWord acc text

splitPart :: Accumulator -> Part -> ReaderWidth Accumulator
-- linebreak: new line no matter what
splitPart (Nothing, rest) LineBreak = pure (Just ([LineBreak], 0), rest)
splitPart (Just (parts, _), rest) LineBreak = pure (Just ([LineBreak], 0), rest Seq.|> parts)
-- whitespace: add to current line no matter what
splitPart (Nothing, rest) (Whitespace text) =
    pure (Just ([Whitespace text], B.textWidth text), rest)
splitPart (Just (parts, lng), rest) (Whitespace text) =
    pure (Just (parts <> [Whitespace text], lng + B.textWidth text), rest)
-- words: split appropriately
splitPart acc (Word text) = splitWord acc text

merge :: Accumulator -> Rows
merge (Nothing, list) = list
merge (Just (parts, _), list) = list Seq.|> parts

toRows :: Parts -> ReaderWidth Rows
toRows parts = merge <$> foldM splitPart (Nothing, Seq.empty) parts

joinRow :: Part -> Text
joinRow (Word text) = text
joinRow (Whitespace text) = text
joinRow LineBreak = ""

rowToText :: Parts -> Text
rowToText row = T.concat $ joinRow <$> row

rowsToText :: Rows -> [Text]
rowsToText rows = toList $ rowToText <$> rows

-- exported
split :: Int -> Text -> Error.EitherError Rows
split width text = do
    parts <- parse text
    pure $ runReader (toRows parts) width

withWidth :: Int -> Text -> Error.EitherError [Text]
withWidth width text = rowsToText <$> split width text