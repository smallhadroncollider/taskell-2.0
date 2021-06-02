module UI.Text.Parser where

import RIO
import qualified RIO.Char as C
import qualified RIO.Text as T

import qualified Data.Attoparsec.Text as AP

data Part
    = Word Text
    | Whitespace Text
    | LineBreak
    deriving (Show, Eq)

notSpace :: Char -> Bool
notSpace c = C.isPrint c && not (C.isSpace c)

whitespace :: AP.Parser Part
whitespace = Whitespace <$> AP.takeWhile1 C.isSpace

newline :: AP.Parser Part
newline = AP.endOfLine $> LineBreak

word :: AP.Parser Part
word = Word <$> AP.takeWhile1 notSpace

parser :: AP.Parser [Part]
parser = AP.many' $ AP.choice [word, newline, whitespace]

parse :: Text -> Either Text [Part]
parse text = first T.pack $ AP.parseOnly (parser <* AP.endOfInput) text
