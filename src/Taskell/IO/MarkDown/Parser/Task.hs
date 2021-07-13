module Taskell.IO.MarkDown.Parser.Task
    ( taskP
    ) where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Types

titleP :: P.Parser Text
titleP = P.string "### " *> P.line

dueP :: Dictionary -> P.Parser Text
dueP dictionary = T.strip <$> (P.string (dictionary ^. duePrefix) *> P.line)

tasksP :: P.Parser AlmostTask
tasksP = do
    _ <- P.string "- ["
    complete <- (== 'x') <$> P.choice [P.char ' ', P.char 'x']
    _ <- P.string "] "
    ttl <- P.line
    pure $ emptyTask & taskTitle .~ ttl & taskComplete .~ complete

afterDescription :: Dictionary -> P.Parser ()
afterDescription dictionary =
    P.choice
        [void tasksP, void (relatedsP dictionary), void (contributorsP dictionary), void titleP]

descriptionP :: Dictionary -> P.Parser Text
descriptionP dictionary = do
    lns <- P.manyTill P.line (P.lookAhead $ afterDescription dictionary)
    pure . T.strip $ T.intercalate "\n" lns

tagP :: P.Parser Text
tagP = P.string "`#" *> P.takeTo "`"

tagsP :: P.Parser [Text]
tagsP = P.lexeme $ tagP `P.sepBy` P.lexeme (P.char ',')

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
        ttl <- titleP
        _ <- optional P.endOfLine
        due <- optional (dueP dictionary)
        _ <- optional P.endOfLine
        description <- optional (descriptionP dictionary)
        tasks <- P.many' tasksP
        tags <- tagsP
        related <- P.option [] $ relatedsP dictionary
        contributors <- P.option [] $ contributorsP dictionary
        pure $
            emptyTask & taskTitle .~ ttl & taskDescription .~ description & taskDue .~ due &
            taskTasks .~ tasks &
            taskTags .~ tags &
            taskRelated .~ related &
            taskContributors .~ contributors
