module Taskell.IO.MarkDown.Serializer.Serialize
    ( serialize
    ) where

import RIO

import qualified RIO.List as L

import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.Taskell as Taskell
import Taskell.IO.MarkDown.Types (Dictionary, contributorsTitle)

type DictionaryReader = Reader Dictionary

type Serializer = DictionaryReader (Maybe Utf8Builder)

s :: Utf8Builder -> Serializer
s = pure . Just

contributorS :: Contributor.Contributor -> DictionaryReader Utf8Builder
contributorS cont = do
    let sign = cont ^. Contributor.sign
    let name = cont ^. Contributor.name
    let email = cont ^. Contributor.email
    pure $ "- @" <> display sign <> ": " <> display name <> " (" <> display email <> ")" <> "\n"

contributorsS :: Taskell.Taskell -> Serializer
contributorsS tsk =
    case tsk ^. Taskell.contributors of
        [] -> pure Nothing
        contributors -> do
            title <- (^. contributorsTitle) <$> ask
            let sorted = L.sortOn (^. Contributor.sign) $ toList contributors
            conts <- traverse contributorS sorted
            s $ "## " <> display title <> "\n\n" <> mconcat conts

descriptionS :: Taskell.Taskell -> Serializer
descriptionS tsk =
    case tsk ^. Taskell.description of
        "" -> pure Nothing
        description -> s $ display description <> "\n"

titleS :: Taskell.Taskell -> Serializer
titleS tsk = s $ "# " <> display (tsk ^. Taskell.title) <> "\n"

serialize' :: Taskell.Taskell -> DictionaryReader Utf8Builder
serialize' tsk = do
    parts <- traverse ($ tsk) [titleS, descriptionS, contributorsS]
    pure $ "" <> mconcat (L.intersperse "\n" (catMaybes parts))

serialize :: Dictionary -> Taskell.Taskell -> Utf8Builder
serialize dic tsk = runReader (serialize' tsk) dic
