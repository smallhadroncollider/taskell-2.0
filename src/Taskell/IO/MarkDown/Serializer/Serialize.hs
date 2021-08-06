module Taskell.IO.MarkDown.Serializer.Serialize
    ( serialize
    ) where

import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

import qualified Taskell.Data.Types.Taskell as Taskell (Taskell)

import qualified Taskell.Error as Error

import Taskell.IO.MarkDown.Convert.ToSerialized (convert)
import Taskell.IO.MarkDown.Types

type DictionaryReader = Reader Dictionary

type Serializer = DictionaryReader (Maybe Utf8Builder)

s :: Utf8Builder -> Serializer
s = pure . Just

eol :: Utf8Builder
eol = "\n"

dbl :: [Utf8Builder] -> [Utf8Builder]
dbl = L.intersperse (eol <> eol)

indent :: Int -> DictionaryReader Utf8Builder
indent i = do
    a <- (^. indentAmount) <$> ask
    pure $ display $ T.replicate (i * a) " "

titleS :: SerializedTaskell -> Serializer
titleS tsk = s $ mconcat ["# ", display (tsk ^. taskellTitle), eol]

descriptionS :: SerializedTaskell -> Serializer
descriptionS tsk =
    case tsk ^. taskellDescription of
        "" -> pure Nothing
        description -> s $ mconcat [display description, eol]

contributorsS :: SerializedTaskell -> Serializer
contributorsS tsk =
    case tsk ^. taskellContributors of
        [] -> pure Nothing
        contributors -> do
            title <- (^. contributorsTitle) <$> ask
            conts <- traverse contributorS contributors
            s $ mconcat ["## ", display title, eol, eol, mconcat conts]

contributorS :: SerializedContributor -> DictionaryReader Utf8Builder
contributorS cont = do
    pure $
        mconcat
            [ "- **@"
            , display (cont ^. contributorSign)
            , "**: "
            , display (cont ^. contributorName)
            , " ("
            , display (cont ^. contributorEmail)
            , ")"
            , eol
            ]

horizontalRuleS :: SerializedTaskell -> Serializer
horizontalRuleS _ = s ("---" <> eol)

listsS :: SerializedTaskell -> Serializer
listsS tsk = do
    case tsk ^. taskellLists of
        [] -> pure Nothing
        lists -> pure . mconcat . dbl <$> traverse listS lists

listS :: SerializedList -> DictionaryReader Utf8Builder
listS lst = do
    let title = lst ^. listTitle
    tasks <- dbl <$> traverse (taskS 0) (lst ^. listTasks)
    pure $ mconcat ["## ", display title, eol, eol, mconcat tasks]

completeS :: Bool -> Utf8Builder
completeS True = "x"
completeS False = " "

taskTitleS :: Int -> SerializedTask -> DictionaryReader Utf8Builder
taskTitleS level task = do
    let title = task ^. taskTitle
    let titlePrefix =
            if level == 0
                then "###"
                else "- [" <> completeS (task ^. taskComplete) <> "]"
    ident <- indent (level - 1)
    pure $ mconcat [ident, titlePrefix, " ", display title, eol]

taskDescriptionS :: SerializedTask -> Serializer
taskDescriptionS task = pure $ display <$> task ^. taskDescription

subTasksS :: Int -> SerializedTask -> Serializer
subTasksS level task =
    case task ^. taskTasks of
        [] -> pure Nothing
        tsks -> pure . mconcat <$> traverse (taskS (level + 1)) tsks

tagsS :: SerializedTask -> Serializer
tagsS task =
    case task ^. taskTags of
        [] -> pure Nothing
        tgs -> s . display . T.intercalate ", " $ ("`#" <>) . (<> "`") <$> tgs

taskContributorS :: SerializedTask -> Serializer
taskContributorS task =
    case task ^. taskContributors of
        [] -> pure Nothing
        cnts -> do
            prefix <- (^. contributorsPrefix) <$> ask
            s . ((display prefix <> " ") <>) . mconcat . L.intersperse ", " $
                display . ("*@" <>) . (<> "*") <$> cnts

relatedLinkS :: (Text, Text, Text) -> Text
relatedLinkS (lTitle, tTitle, lnk) = mconcat ["[", lTitle, " / ", tTitle, "](#", lnk, ")"]

relatedS :: SerializedTask -> Serializer
relatedS task =
    case task ^. taskRelated of
        [] -> pure Nothing
        rels -> do
            prefix <- (^. relatedPrefix) <$> ask
            s . ((display prefix <> " ") <>) . mconcat . L.intersperse ", " $
                display . relatedLinkS <$> rels

idd :: Int -> (SerializedTask -> Serializer) -> SerializedTask -> Serializer
idd level fn task = do
    ident <- indent level
    utf8B <- fn task
    pure $ (ident <>) <$> utf8B

taskS :: Int -> SerializedTask -> DictionaryReader Utf8Builder
taskS level task = do
    let ident = idd level
    let parts =
            [ ident taskDescriptionS
            , subTasksS level
            , ident tagsS
            , ident relatedS
            , ident taskContributorS
            ]
    main <- dbl . catMaybes <$> sequence (($ task) <$> parts)
    title <- taskTitleS level task
    pure $ mconcat [title, eol, mconcat main]

serialize' :: SerializedTaskell -> DictionaryReader Utf8Builder
serialize' tsk = do
    parts <- traverse ($ tsk) [titleS, descriptionS, contributorsS, horizontalRuleS, listsS]
    pure $ "" <> mconcat (L.intersperse eol (catMaybes parts))

serialize :: Dictionary -> Taskell.Taskell -> Error.EitherError Utf8Builder
serialize dic tsk = do
    tsk' <- convert tsk
    pure $ runReader (serialize' tsk') dic
