module Taskell.UI.Text.Parser where

import RIO
import qualified RIO.Char as C
import qualified RIO.Text as T

import qualified Data.Attoparsec.Text as AP

import qualified Error

data Part
    = Word Text
    | Whitespace Text
    | LineBreak
    deriving (Show, Eq)

type Parts = [Part]

notSpace :: Char -> Bool
notSpace c = C.isPrint c && not (C.isSpace c)

space :: Char -> Bool
space c = C.isSpace c && not (AP.isEndOfLine c)

whitespace :: AP.Parser Part
whitespace = Whitespace <$> AP.takeWhile1 space

newline :: AP.Parser Part
newline = AP.endOfLine $> LineBreak

word :: AP.Parser Part
word = Word <$> AP.takeWhile1 notSpace

parser :: AP.Parser [Part]
parser = AP.many' $ AP.choice [word, newline, whitespace]

parse :: Text -> Error.EitherError [Part]
parse text = first (Error.txt . T.pack) $ AP.parseOnly (parser <* AP.endOfInput) text
