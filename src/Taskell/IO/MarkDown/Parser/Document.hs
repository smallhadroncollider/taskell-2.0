module Taskell.IO.MarkDown.Parser.Document where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Task
import Taskell.IO.MarkDown.Parser.Utility (titleP)
import Taskell.IO.MarkDown.Types

descriptionP :: Text -> P.Parser Text
descriptionP cTitle =
    T.intercalate "\n" <$> P.lexeme (P.manyTill P.line (contributorsTitleP cTitle))

contributorsTitleP :: Text -> P.Parser ()
contributorsTitleP cTitle = void . P.lexeme $ P.string ("## " <> cTitle)

contributorP :: P.Parser SerializedContributor
contributorP = do
    _ <- P.string "- **@"
    sign <- P.takeTo "**:"
    name <- T.strip <$> P.takeTo "("
    email <- P.takeTo ")"
    _ <- P.endOfLine
    pure $ SerializedContributor sign name email

contributorsP :: P.Parser [SerializedContributor]
contributorsP = P.many' contributorP

hrP :: P.Parser ()
hrP = void . P.lexeme $ P.string "---"

listP :: Dictionary -> P.Parser SerializedList
listP dictionary =
    P.lexeme $ do
        ttl <- titleP 2
        tasks <- P.many1' (taskP dictionary)
        pure $ SerializedList ttl tasks

listsP :: Dictionary -> P.Parser [SerializedList]
listsP dictionary = P.many' (listP dictionary)

parser :: Dictionary -> P.Parser SerializedTaskell
parser dictionary = do
    t <- titleP 1
    d <- descriptionP (dictionary ^. contributorsTitle)
    c <- contributorsP <* hrP
    l <- listsP dictionary
    pure $ SerializedTaskell t d c l

parse :: Dictionary -> Text -> Either Text SerializedTaskell
parse dictionary text = first T.pack $ P.parseOnly (parser dictionary <* P.endOfInput) text
