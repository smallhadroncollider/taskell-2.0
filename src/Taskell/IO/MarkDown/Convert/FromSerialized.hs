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

import Taskell.IO.MarkDown.Convert.GitHubLinks (generateLinks)
import Taskell.IO.MarkDown.Parser.Document (parse)
import Taskell.IO.MarkDown.Types

import Taskell.Utility.Tuple (thrd)

-- conversion
setDescription :: Maybe Text -> Task.TaskID -> Taskell.Taskell -> Error.EitherError Taskell.Taskell
setDescription Nothing _ tsk = pure tsk
setDescription (Just desc) tID tsk = Taskell.changeTaskDescription desc tID tsk

type Relation = (Task.TaskID, [Related])

type WithRelationships = (Taskell.Taskell, [Relation])

addRelationships' ::
       HashMap Text Task.TaskID -> Taskell.Taskell -> Relation -> Error.EitherError Taskell.Taskell
addRelationships' linkMap tsk (tID, rels) = do
    rels' <- Error.mEither "Unknown link" $ traverse (`HM.lookup` linkMap) (thrd <$> rels)
    let zips = (tID, ) <$> rels'
    foldM (flip Taskell.addTaskRelationship) tsk zips

addRelationships :: WithRelationships -> Error.EitherError Taskell.Taskell
addRelationships (tsk, rels) = do
    let lists = tsk ^. Taskell.listsOrder
    tsks <- traverse (`Taskell.tasksForListWithIDs` tsk) lists
    let flat = join tsks
    let links = generateLinks $ (^. Task.title) . snd <$> flat
    let linkMap = HM.fromList $ zip (toList links) (toList $ fst <$> flat)
    foldM (addRelationships' linkMap) tsk rels

serialisedTaskToTaskell ::
       Task.TaskID -> WithRelationships -> SerializedTask -> Error.EitherError WithRelationships
serialisedTaskToTaskell taskID (tsk, rels) t = do
    let conts =
            Seq.fromList . catMaybes $
            Taskell.findContributorFromSign tsk <$> (t ^. taskContributors)
    updated <-
        Taskell.setTaskContributors conts taskID tsk >>=
        setDescription (t ^. taskDescription) taskID >>=
        Taskell.changeTaskCompleted (t ^. taskComplete) taskID
    updated' <- foldM (flip (Taskell.addTagToTask taskID)) updated (t ^. taskTags)
    let rels' =
            if null (t ^. taskRelated)
                then rels
                else rels <> [(taskID, t ^. taskRelated)]
    foldM (serialisedTaskToTaskellTask taskID) (updated', rels') (t ^. taskTasks)

serialisedTaskToTaskellTask ::
       Task.TaskID -> WithRelationships -> SerializedTask -> Error.EitherError WithRelationships
serialisedTaskToTaskellTask parentID (tsk, rels) t = do
    (taskID, updated) <- Taskell.addTaskToTask (t ^. taskTitle) parentID tsk
    serialisedTaskToTaskell taskID (updated, rels) t

serialisedTaskToTaskellList ::
       List.ListID -> WithRelationships -> SerializedTask -> Error.EitherError WithRelationships
serialisedTaskToTaskellList parentID (tsk, rels) t = do
    (taskID, updated) <- Taskell.addTaskToList (t ^. taskTitle) parentID tsk
    serialisedTaskToTaskell taskID (updated, rels) t

serialisedListToTaskell ::
       WithRelationships -> SerializedList -> Error.EitherError WithRelationships
serialisedListToTaskell (tsk, rels) l = do
    (listID, updated) <- Taskell.addList (l ^. listTitle) tsk
    foldM (serialisedTaskToTaskellList listID) (updated, rels) (l ^. listTasks)

serialisedListsToTaskell ::
       Taskell.Taskell -> [SerializedList] -> Error.EitherError WithRelationships
serialisedListsToTaskell tsk = foldM serialisedListToTaskell (tsk, [])

serialisedContributorToContributor :: SerializedContributor -> Contributor.Contributor
serialisedContributorToContributor serialised =
    Contributor.Contributor
        (serialised ^. contributorSign)
        (serialised ^. contributorName)
        (serialised ^. contributorEmail)

contributors :: [SerializedContributor] -> [Contributor.Contributor]
contributors = (serialisedContributorToContributor <$>)

serialisedToTaskell :: SerializedTaskell -> Error.EitherError Taskell.Taskell
serialisedToTaskell serialised = do
    let tsk =
            Taskell.create (serialised ^. taskellTitle) &
            Taskell.description .~ (serialised ^. taskellDescription)
    let tsk' =
            foldl' (\a c -> snd $ Taskell.addContributor c a) tsk $
            contributors (serialised ^. taskellContributors)
    serialisedListsToTaskell tsk' (serialised ^. taskellLists) >>= addRelationships

convert :: Dictionary -> Text -> Error.EitherError Taskell.Taskell
convert dictionary input = either Error.e serialisedToTaskell $ parse dictionary input
