module Taskell.IO.MarkDown.Parser.Document where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.Taskell as Taskell
import qualified Taskell.Error as Error

import Taskell.IO.MarkDown.Parser.Convert
import Taskell.IO.MarkDown.Parser.Task
import Taskell.IO.MarkDown.Parser.Types

descriptionP :: Text -> P.Parser Text
descriptionP cTitle =
    T.intercalate "\n" <$> P.lexeme (P.manyTill P.line (contributorsTitleP cTitle))

contributorsTitleP :: Text -> P.Parser ()
contributorsTitleP cTitle = void . P.lexeme $ P.string ("## " <> cTitle)

contributorP :: P.Parser Contributor.Contributor
contributorP = do
    _ <- P.string "- **@"
    sign <- P.takeTo "**:"
    name <- T.strip <$> P.takeTo "("
    email <- P.takeTo ")"
    _ <- P.endOfLine
    pure $ Contributor.Contributor sign name email

contributorsP :: P.Parser Contributor.Contributors
contributorsP = do
    cs <- P.many' contributorP
    pure $ HM.fromList (zip (Contributor.ContributorID <$> [1 ..]) cs)

hrP :: P.Parser ()
hrP = void . P.lexeme $ P.string "---"

listP :: Dictionary -> P.Parser AlmostList
listP dictionary =
    P.lexeme $ do
        ttl <- titleP 2
        tasks <- P.many1' (taskP dictionary)
        pure $ AlmostList ttl tasks

listsP :: Dictionary -> Taskell.Taskell -> P.Parser (Error.EitherError Taskell.Taskell)
listsP dictionary tsk = almostsToTaskell tsk <$> P.many' (listP dictionary)

parser :: Dictionary -> P.Parser (Error.EitherError Taskell.Taskell)
parser dictionary = do
    t <- titleP 1
    d <- descriptionP (dictionary ^. contributorsTitle)
    c <- contributorsP <* hrP
    listsP dictionary $ Taskell.create t & Taskell.description .~ d & Taskell.contributors .~ c

parse :: Text -> Dictionary -> Error.EitherError Taskell.Taskell
parse text dictionary =
    either (Error.e . T.pack) id $ P.parseOnly (parser dictionary <* P.endOfInput) text
