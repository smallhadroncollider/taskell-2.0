module Taskell.UI.Text.Parser where

import RIO
import qualified RIO.Char as C
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import qualified Taskell.Error as Error

data Part
    = Word Text
    | Whitespace Text
    | LineBreak
    deriving (Show, Eq)

type Parts = [Part]

notSpace :: Char -> Bool
notSpace c = C.isPrint c && not (C.isSpace c)

space :: Char -> Bool
space c = C.isSpace c && not (P.isEndOfLine c)

whitespace :: P.Parser Part
whitespace = Whitespace <$> P.takeWhile1 space

newline :: P.Parser Part
newline = P.endOfLine $> LineBreak

word :: P.Parser Part
word = Word <$> P.takeWhile1 notSpace

parser :: P.Parser [Part]
parser = P.many' $ P.choice [word, newline, whitespace]

parse :: Text -> Error.EitherError [Part]
parse text = first (Error.txt . T.pack) $ P.parseOnly (parser <* P.endOfInput) text
