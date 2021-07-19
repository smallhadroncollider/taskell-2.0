module Taskell.IO.MarkDown.Serializer.Serialize
    ( serialize
    ) where

import RIO

import qualified RIO.List as L

import qualified Taskell.Data.Types.Taskell as Taskell (Taskell)

import Taskell.IO.MarkDown.Convert.ToSerialized (convert)
import Taskell.IO.MarkDown.Types

type DictionaryReader = Reader Dictionary

type Serializer = DictionaryReader (Maybe Utf8Builder)

s :: Utf8Builder -> Serializer
s = pure . Just

contributorS :: SerializedContributor -> DictionaryReader Utf8Builder
contributorS cont = do
    let sign = cont ^. contributorSign
    let name = cont ^. contributorName
    let email = cont ^. contributorEmail
    pure $ "- @" <> display sign <> ": " <> display name <> " (" <> display email <> ")" <> "\n"

contributorsS :: SerializedTaskell -> Serializer
contributorsS tsk =
    case tsk ^. taskellContributors of
        [] -> pure Nothing
        contributors -> do
            title <- (^. contributorsTitle) <$> ask
            conts <- traverse contributorS contributors
            s $ "## " <> display title <> "\n\n" <> mconcat conts

descriptionS :: SerializedTaskell -> Serializer
descriptionS tsk =
    case tsk ^. taskellDescription of
        "" -> pure Nothing
        description -> s $ display description <> "\n"

titleS :: SerializedTaskell -> Serializer
titleS tsk = s $ "# " <> display (tsk ^. taskellTitle) <> "\n"

serialize' :: SerializedTaskell -> DictionaryReader Utf8Builder
serialize' tsk = do
    parts <- traverse ($ tsk) [titleS, descriptionS, contributorsS]
    pure $ "" <> mconcat (L.intersperse "\n" (catMaybes parts))

serialize :: Dictionary -> Taskell.Taskell -> Utf8Builder
serialize dic tsk = runReader (serialize' tsk') dic
  where
    tsk' = convert tsk
