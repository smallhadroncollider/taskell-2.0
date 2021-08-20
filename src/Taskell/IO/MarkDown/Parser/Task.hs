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

-- parsers
dueP :: IndentedParser Text
dueP dictionary level =
    P.stripEmptyLines $ do
        _ <- indent dictionary level
        date <- P.string (dictionary ^. duePrefix) *> P.line
        pure $ T.strip date

afterDescription :: IndentedParser ()
afterDescription dictionary level =
    P.choice
        [ void (taskP dictionary level) -- same task block
        , void (taskP dictionary (level + 1)) -- sub-task block
        , void (tagsP dictionary level) -- related block
        , void (tagsP dictionary (level - 1)) -- related block
        , void (relatedsP dictionary level) -- related block
        , void (relatedsP dictionary (level - 1)) -- related block
        , void (contributorsP dictionary level) -- contributors block
        , void (contributorsP dictionary (level - 1)) -- contributors block
        , void (titleP 3) -- task title
        , void (titleP 2) -- list title
        , void P.endOfInput -- end of file
        ]

descriptionLineP :: IndentedParser Text
descriptionLineP dictionary level = P.choice [indent dictionary level *> P.line, "" <$ P.endOfLine]

descriptionP :: IndentedParser Text
descriptionP dictionary level = do
    lns <-
        P.manyTill
            (descriptionLineP dictionary level)
            (P.lookAhead $ afterDescription dictionary level)
    pure . T.strip $ T.intercalate "\n" lns

tagP :: P.Parser Text
tagP = P.string "`#" *> P.takeTill (== '`') <* P.string "`"

tagsP :: IndentedParser [Text]
tagsP dictionary level =
    P.stripEmptyLines $ do
        _ <- indent dictionary level
        tagP `P.sepBy1` P.lexeme (P.char ',')

relatedP :: P.Parser Related
relatedP = do
    lTitle <- T.strip <$> (P.string "[" *> P.takeTill (== '/') <* P.string "/")
    tTitle <- T.strip <$> P.takeTill (== ']') <* P.string "](#"
    lnk <- P.takeTill (== ')') <* P.string ")"
    pure (lTitle, tTitle, lnk)

relatedsP :: IndentedParser [Related]
relatedsP dictionary level =
    P.stripEmptyLines $ do
        _ <- indent dictionary level
        _ <- P.string (dictionary ^. relatedPrefix) <* P.string " "
        relatedP `P.sepBy1` P.lexeme (P.char ',')

contributorsP :: IndentedParser [Text]
contributorsP dictionary level =
    P.stripEmptyLines $ do
        _ <- indent dictionary level
        _ <- P.string (dictionary ^. contributorsPrefix) <* P.string " "
        (P.string "*@" *> P.takeTill (== '*') <* P.string "*") `P.sepBy1` P.lexeme (P.char ',')

taskTitleP :: IndentedParser (Bool, Text)
taskTitleP _ 0 = P.stripEmptyLines $ (False, ) <$> titleP 3
taskTitleP dictionary level =
    P.stripEmptyLines $ do
        _ <- indent dictionary (level - 1)
        _ <- P.string "- ["
        complete <- (== 'x') <$> P.choice [P.char ' ', P.char 'x']
        _ <- P.string "] "
        ttl <- P.line
        pure (complete, ttl)

taskP :: IndentedParser SerializedTask
taskP dictionary level =
    P.stripEmptyLines $ do
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
