module Taskell.IO.MarkDown.Parser.Document where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Task
import Taskell.IO.MarkDown.Parser.Types
import Taskell.IO.MarkDown.Parser.Utility (titleP)
import Taskell.IO.MarkDown.Types

descriptionP :: Text -> P.Parser Text
descriptionP cTitle =
    T.intercalate "\n" <$> P.lexeme (P.manyTill P.line (contributorsTitleP cTitle))

contributorsTitleP :: Text -> P.Parser ()
contributorsTitleP cTitle = void . P.lexeme $ P.string ("## " <> cTitle)

contributorP :: P.Parser ParsedContributor
contributorP = do
    _ <- P.string "- **@"
    sign <- P.takeTo "**:"
    name <- T.strip <$> P.takeTo "("
    email <- P.takeTo ")"
    _ <- P.endOfLine
    pure $ ParsedContributor sign name email

contributorsP :: P.Parser [ParsedContributor]
contributorsP = P.many' contributorP

hrP :: P.Parser ()
hrP = void . P.lexeme $ P.string "---"

listP :: Dictionary -> P.Parser ParsedList
listP dictionary =
    P.lexeme $ do
        ttl <- titleP 2
        tasks <- P.many1' (taskP dictionary)
        pure $ ParsedList ttl tasks

listsP :: Dictionary -> P.Parser [ParsedList]
listsP dictionary = P.many' (listP dictionary)

parser :: Dictionary -> P.Parser ParsedTaskell
parser dictionary = do
    t <- titleP 1
    d <- descriptionP (dictionary ^. contributorsTitle)
    c <- contributorsP <* hrP
    l <- listsP dictionary
    pure $ ParsedTaskell t d c l

parse :: Dictionary -> Text -> Either Text ParsedTaskell
parse dictionary text = first T.pack $ P.parseOnly (parser dictionary <* P.endOfInput) text
