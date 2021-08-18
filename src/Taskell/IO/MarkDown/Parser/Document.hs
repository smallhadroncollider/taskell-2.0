module Taskell.IO.MarkDown.Parser.Document where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Task
import Taskell.IO.MarkDown.Parser.Utility (titleP)
import Taskell.IO.MarkDown.Types

descriptionP :: Text -> P.Parser Text
descriptionP cTitle =
    T.intercalate "\n" <$>
    P.stripEmptyLines (P.manyTill P.line (contributorsTitleP cTitle <|> P.lookAhead hrP))

contributorsTitleP :: Text -> P.Parser ()
contributorsTitleP cTitle = void . P.stripEmptyLines $ P.string ("## " <> cTitle)

contributorP :: P.Parser SerializedContributor
contributorP =
    SerializedContributor <$> (P.string "- **@" *> P.takeTill (== '*') <* P.string "**:") <*>
    (T.strip <$> P.takeTill (== '(') <* P.string "(") <*>
    (P.takeTill (== ')') <* P.string ")" <* P.endOfLine)

contributorsP :: P.Parser [SerializedContributor]
contributorsP = P.many' contributorP

hrP :: P.Parser ()
hrP = void . P.stripEmptyLines $ P.string "---"

listP :: Dictionary -> P.Parser SerializedList
listP dictionary = P.stripEmptyLines $ SerializedList <$> titleP 2 <*> P.many' (taskP dictionary 0)

listsP :: Dictionary -> P.Parser [SerializedList]
listsP dictionary = P.many' (listP dictionary)

parser :: Dictionary -> P.Parser SerializedTaskell
parser dictionary =
    SerializedTaskell <$> titleP 1 <*> descriptionP (dictionary ^. contributorsTitle) <*>
    (contributorsP <* hrP) <*>
    listsP dictionary

parse :: Dictionary -> Text -> Either Text SerializedTaskell
parse dictionary text = first T.pack $ P.parseOnly (parser dictionary <* P.endOfInput) text
