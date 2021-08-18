module Taskell.Utility.Parser
    ( lexeme
    , line
    , stripEmptyLines
    , module Data.Attoparsec.Combinator
    , module Data.Attoparsec.Text
    ) where

import RIO

import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

stripEmptyLines :: Parser a -> Parser a
stripEmptyLines p = many' endOfLine *> p <* many' endOfLine

line :: Parser Text
line = takeTill isEndOfLine <* endOfLine
