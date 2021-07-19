module Taskell.IO.MarkDown.Serializer.Serialize
    ( serialize
    ) where

import RIO

import qualified RIO.List as L

import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.Taskell as Taskell
import Taskell.IO.MarkDown.Types (Dictionary, contributorsTitle)

type DictionaryReader = Reader Dictionary

contributorS :: Contributor.Contributor -> DictionaryReader Utf8Builder
contributorS cont = do
    let sign = cont ^. Contributor.sign
    let name = cont ^. Contributor.name
    let email = cont ^. Contributor.email
    pure $ "- @" <> display sign <> ": " <> display name <> " (" <> display email <> ")" <> "\n"

contributorsS :: Taskell.Taskell -> DictionaryReader Utf8Builder
contributorsS tsk = do
    title <- (^. contributorsTitle) <$> ask
    let sorted = L.sortOn (^. Contributor.sign) $ toList (tsk ^. Taskell.contributors)
    conts <- traverse contributorS sorted
    pure $ "## " <> display title <> "\n\n" <> mconcat conts

descriptionS :: Taskell.Taskell -> DictionaryReader Utf8Builder
descriptionS tsk = pure $ display description <> "\n"
  where
    description = tsk ^. Taskell.description

titleS :: Taskell.Taskell -> DictionaryReader Utf8Builder
titleS tsk = pure $ "# " <> display (tsk ^. Taskell.title) <> "\n"

blankS :: Taskell.Taskell -> DictionaryReader Utf8Builder
blankS _ = pure $ "\n"

serialize' :: Taskell.Taskell -> DictionaryReader Utf8Builder
serialize' tsk = do
    parts <- traverse ($ tsk) [titleS, blankS, descriptionS, blankS, contributorsS]
    pure $ Utf8Builder "" <> mconcat parts

serialize :: Dictionary -> Taskell.Taskell -> Utf8Builder
serialize dic tsk = runReader (serialize' tsk) dic
