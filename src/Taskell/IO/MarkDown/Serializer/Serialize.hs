module Taskell.IO.MarkDown.Serializer.Serialize
    ( serialize
    ) where

import RIO

import qualified RIO.List as L

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

contributorS :: SerializedContributor -> DictionaryReader Utf8Builder
contributorS cont = do
    pure $
        mconcat
            [ "- @"
            , display (cont ^. contributorSign)
            , ": "
            , display (cont ^. contributorName)
            , " ("
            , display (cont ^. contributorEmail)
            , ")"
            , eol
            ]

contributorsS :: SerializedTaskell -> Serializer
contributorsS tsk =
    case tsk ^. taskellContributors of
        [] -> pure Nothing
        contributors -> do
            title <- (^. contributorsTitle) <$> ask
            conts <- traverse contributorS contributors
            s $ mconcat ["## ", display title, eol, eol, mconcat conts]

descriptionS :: SerializedTaskell -> Serializer
descriptionS tsk =
    case tsk ^. taskellDescription of
        "" -> pure Nothing
        description -> s $ mconcat [display description, eol]

titleS :: SerializedTaskell -> Serializer
titleS tsk = s $ mconcat ["# ", display (tsk ^. taskellTitle), eol]

serialize' :: SerializedTaskell -> DictionaryReader Utf8Builder
serialize' tsk = do
    parts <- traverse ($ tsk) [titleS, descriptionS, contributorsS]
    pure $ "" <> mconcat (L.intersperse eol (catMaybes parts))

serialize :: Dictionary -> Taskell.Taskell -> Error.EitherError Utf8Builder
serialize dic tsk = do
    tsk' <- convert tsk
    pure $ runReader (serialize' tsk') dic
