module Taskell.Utility.Parser
    ( lexeme
    , takeTo
    , line
    , module Data.Attoparsec.Combinator
    , module Data.Attoparsec.Text
    ) where

import RIO
import qualified RIO.Text as T

import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

takeTo :: Text -> Parser Text
takeTo text =
    case T.uncons text of
        Nothing -> pure ""
        Just (ch, rest) -> do
            takeTill (== ch) <* string (T.singleton ch) <* string rest

line :: Parser Text
line = takeTill isEndOfLine <* endOfLine
