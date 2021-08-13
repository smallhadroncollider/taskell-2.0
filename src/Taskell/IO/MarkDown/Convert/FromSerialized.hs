{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Convert.FromSerialized
    ( convert
    ) where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Seq as Seq

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Data.Types.Taskell as Taskell
import qualified Taskell.Error as Error

import Taskell.IO.MarkDown.Parser.Document (parse)
import Taskell.IO.MarkDown.Types

-- conversion
setDescription :: Maybe Text -> Taskell.Taskell -> Task.TaskID -> Error.EitherError Taskell.Taskell
setDescription Nothing tsk _ = pure tsk
setDescription (Just desc) tsk tID = Taskell.changeTaskDescription desc tID tsk

parsedTaskToTaskell ::
       Task.TaskID -> Taskell.Taskell -> SerializedTask -> Error.EitherError Taskell.Taskell
parsedTaskToTaskell taskID tsk t = do
    let conts =
            Seq.fromList . catMaybes $
            Taskell.findContributorFromSign tsk <$> (t ^. taskContributors)
    updated <- Taskell.setTaskContributors conts taskID tsk
    updated' <- setDescription (t ^. taskDescription) updated taskID
    updated'' <- Taskell.changeTaskCompleted (t ^. taskComplete) taskID updated'
    updated''' <- foldM (flip (Taskell.addTagToTask taskID)) updated'' (t ^. taskTags)
    foldM (parsedTaskToTaskellTask taskID) updated''' (t ^. taskTasks)

parsedTaskToTaskellTask ::
       Task.TaskID -> Taskell.Taskell -> SerializedTask -> Error.EitherError Taskell.Taskell
parsedTaskToTaskellTask parentID tsk t = do
    (taskID, updated) <- Taskell.addTaskToTask (t ^. taskTitle) parentID tsk
    parsedTaskToTaskell taskID updated t

parsedTaskToTaskellList ::
       List.ListID -> Taskell.Taskell -> SerializedTask -> Error.EitherError Taskell.Taskell
parsedTaskToTaskellList parentID tsk t = do
    (taskID, updated) <- Taskell.addTaskToList (t ^. taskTitle) parentID tsk
    parsedTaskToTaskell taskID updated t

parsedListToTaskell :: Taskell.Taskell -> SerializedList -> Error.EitherError Taskell.Taskell
parsedListToTaskell tsk l = do
    (listID, updated) <- Taskell.addList (l ^. listTitle) tsk
    foldM (parsedTaskToTaskellList listID) updated (l ^. listTasks)

parsedListsToTaskell :: Taskell.Taskell -> [SerializedList] -> Error.EitherError Taskell.Taskell
parsedListsToTaskell = foldM parsedListToTaskell

parsedContributorToContributor :: SerializedContributor -> Contributor.Contributor
parsedContributorToContributor parsed =
    Contributor.Contributor
        (parsed ^. contributorSign)
        (parsed ^. contributorName)
        (parsed ^. contributorEmail)

contributors :: [SerializedContributor] -> Contributor.Contributors
contributors parsed = HM.fromList (zip ids cs)
  where
    cs = parsedContributorToContributor <$> parsed
    ids = Contributor.ContributorID <$> [1 ..]

parsedToTaskell :: SerializedTaskell -> Error.EitherError Taskell.Taskell
parsedToTaskell parsed = do
    let tsk =
            Taskell.create (parsed ^. taskellTitle) &
            Taskell.description .~ (parsed ^. taskellDescription) &
            Taskell.contributors .~ contributors (parsed ^. taskellContributors)
    parsedListsToTaskell tsk (parsed ^. taskellLists)

convert :: Dictionary -> Text -> Error.EitherError Taskell.Taskell
convert dictionary input = either Error.e parsedToTaskell $ parse dictionary input
