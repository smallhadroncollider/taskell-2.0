module Taskell.IO.MarkDown.Parser.Task
    ( taskP
    ) where

import RIO
import qualified RIO.Text as T

import qualified Taskell.Utility.Parser as P

import Taskell.IO.MarkDown.Parser.Utility (titleP)
import Taskell.IO.MarkDown.Types

type IndentedParser a = Dictionary -> Int -> P.Parser a

-- helpers
indent :: IndentedParser Text
indent _ 0 = pure ""
indent dictionary level = P.string (T.replicate num " ")
  where
    num = (dictionary ^. indentAmount) * level

stripEmptyLines :: P.Parser a -> P.Parser a
stripEmptyLines p = P.many' P.endOfLine *> p

-- parsers
dueP :: IndentedParser Text
dueP dictionary level =
    stripEmptyLines $ do
        _ <- indent dictionary level
        date <- P.string (dictionary ^. duePrefix) *> P.line
        pure $ T.strip date

afterDescription :: IndentedParser ()
afterDescription dictionary level =
    P.choice
        [ void (taskP dictionary level) -- same task block
        , void (taskP dictionary (level + 1)) -- sub-task block
        , void (tagsP dictionary level) -- related block
        , void (relatedsP dictionary level) -- related block
        , void (contributorsP dictionary level) -- contributors block
        , void (titleP 3) -- task title
        , void (titleP 2) -- list title
        , void P.endOfInput -- end of file
        ]

descriptionP :: IndentedParser Text
descriptionP dictionary level =
    stripEmptyLines $ do
        _ <- indent dictionary level
        lns <- P.manyTill P.line (P.lookAhead $ afterDescription dictionary level)
        pure . T.strip $ T.intercalate "\n" lns

tagP :: P.Parser Text
tagP = P.string "`#" *> P.takeTo "`"

tagsP :: IndentedParser [Text]
tagsP dictionary level =
    stripEmptyLines $ do
        _ <- indent dictionary level
        tagP `P.sepBy1` P.lexeme (P.char ',')

relatedP :: P.Parser Related
relatedP = do
    lTitle <- P.string "[" *> P.takeTo " / "
    tTitle <- P.takeTo "](#"
    lnk <- P.takeTo ")"
    pure (lTitle, tTitle, lnk)

relatedsP :: IndentedParser [Related]
relatedsP dictionary level =
    stripEmptyLines $ do
        _ <- indent dictionary level
        _ <- P.string (dictionary ^. relatedPrefix) <* P.string " "
        relatedP `P.sepBy1` P.lexeme (P.char ',')

contributorsP :: IndentedParser [Text]
contributorsP dictionary level =
    stripEmptyLines $ do
        _ <- indent dictionary level
        _ <- P.string (dictionary ^. contributorsPrefix) <* P.string " "
        (P.string "*@" *> P.takeTo "*") `P.sepBy1` P.lexeme (P.char ',')

taskTitleP :: IndentedParser (Bool, Text)
taskTitleP _ 0 = stripEmptyLines $ (False, ) <$> titleP 3
taskTitleP dictionary level =
    stripEmptyLines $ do
        _ <- indent dictionary (level - 1)
        _ <- P.string "- ["
        complete <- (== 'x') <$> P.choice [P.char ' ', P.char 'x']
        _ <- P.string "] "
        ttl <- P.line
        pure (complete, ttl)

taskP :: IndentedParser SerializedTask
taskP dictionary level =
    stripEmptyLines $ do
        (complete, ttl) <- taskTitleP dictionary level
        _ <- optional (dueP dictionary level)
        description <- optional (descriptionP dictionary level)
        tasks <- P.many' (taskP dictionary (level + 1))
        tags <- P.option [] (tagsP dictionary level)
        related <- P.option [] (relatedsP dictionary level)
        contributors <- P.option [] (contributorsP dictionary level)
        let desc =
                if description == Just ""
                    then Nothing
                    else description
        pure $
            emptyTask & taskComplete .~ complete & taskTitle .~ ttl & taskDescription .~ desc &
            taskTasks .~ tasks &
            taskTags .~ tags &
            taskRelated .~ related &
            taskContributors .~ contributors
