module UI.Text.Split
    ( withWidth
    ) where

import RIO
import qualified RIO.Text as T

import qualified Brick.Widgets.Core as B (textWidth)

import UI.Text.Parser

type ReaderWidth = Reader Int

type Accumulator = (Text, [Text])

splitLongWord :: Accumulator -> Text -> ReaderWidth Accumulator
splitLongWord (current, rest) text = do
    width <- ask
    let (firstLine, secondLine) = T.splitAt width text
    let rest' = rest <> filter (/= "") [current, firstLine]
    if B.textWidth secondLine > width
        then splitLongWord ("", rest') secondLine
        else pure (secondLine, rest')

joinWord :: Accumulator -> Text -> ReaderWidth Accumulator
joinWord (current, rest) text = do
    let joined = current <> text
    width <- ask
    pure $
        if B.textWidth joined > width
            then (text, rest <> [current])
            else (joined, rest)

splitWord :: Accumulator -> Text -> ReaderWidth Accumulator
splitWord acc text = do
    width <- ask
    if B.textWidth text > width
        then splitLongWord acc text
        else joinWord acc text

splitPart :: Accumulator -> Part -> ReaderWidth Accumulator
splitPart (current, rest) LineBreak = pure ("", rest <> [current])
splitPart (current, rest) (Whitespace text) = pure (current <> text, rest)
splitPart acc (Word text) = splitWord acc text

merge :: (a, [a]) -> [a]
merge (item, list) = list <> [item]

split :: [Part] -> ReaderWidth [Text]
split parts = merge <$> foldM splitPart ("", []) parts

withWidth :: Int -> Text -> Either Text [Text]
withWidth width text = do
    parts <- parse text
    pure $ runReader (split parts) width
