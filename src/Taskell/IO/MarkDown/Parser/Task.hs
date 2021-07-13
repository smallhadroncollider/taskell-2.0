module Taskell.IO.MarkDown.Parser.Task
    ( taskP
    ) where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Types

dueP :: Dictionary -> P.Parser Text
dueP dictionary = T.strip <$> (P.string (dictionary ^. duePrefix) *> P.line)

tasksP :: P.Parser AlmostTask
tasksP = do
    _ <- P.string "- ["
    complete <- (== 'x') <$> P.choice [P.char ' ', P.char 'x']
    _ <- P.string "] "
    ttl <- P.line
    pure $ emptyTask & taskTitle .~ ttl & taskComplete .~ complete

descriptionP :: P.Parser Text
descriptionP = do
    let nextBlock = [tasksP]
    lns <- P.manyTill P.line (P.lookAhead (P.choice nextBlock))
    pure . T.strip $ T.intercalate "\n" lns

tagP :: P.Parser Text
tagP = P.string "`#" *> P.takeTo "`"

tagsP :: P.Parser [Text]
tagsP = tagP `P.sepBy` P.lexeme (P.char ',')

relatedP :: P.Parser Text
relatedP = P.string "[" <* P.takeTo "](#" *> P.takeTo ")"

relatedsP :: Dictionary -> P.Parser [Text]
relatedsP dictionary =
    P.lexeme $ do
        _ <- P.lexeme $ P.string (dictionary ^. relatedPrefix)
        relatedP `P.sepBy` P.lexeme (P.char ',')

contributorsP :: Dictionary -> P.Parser [Text]
contributorsP dictionary =
    P.lexeme $ do
        _ <- P.lexeme $ P.string (dictionary ^. contributorsPrefix)
        (P.string "*@" *> P.takeTo "*") `P.sepBy` P.lexeme (P.char ',')

taskP :: Dictionary -> P.Parser AlmostTask
taskP dictionary =
    P.lexeme $ do
        ttl <- P.string "### " *> P.line <* P.endOfLine
        due <- dueP dictionary <* P.endOfLine
        description <- descriptionP
        tasks <- P.many' tasksP <* P.endOfLine
        tags <- tagsP
        related <- relatedsP dictionary
        contributors <- contributorsP dictionary
        pure $
            emptyTask & taskTitle .~ ttl & taskDescription .~ description & taskDue .~ due &
            taskTasks .~ tasks &
            taskTags .~ tags &
            taskRelated .~ related &
            taskContributors .~ contributors
