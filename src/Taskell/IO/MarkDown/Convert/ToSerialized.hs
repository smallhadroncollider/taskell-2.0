{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Convert.ToSerialized
    ( convert
    , relatedDictionary
    ) where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import qualified RIO.Seq as Seq

import qualified Taskell.Error as Error

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Tag as Tag
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Data.Types.Taskell as Taskell

import Taskell.IO.MarkDown.Convert.GitHubLinks (generateLinks)
import Taskell.IO.MarkDown.Types

emptyStringToNothing :: Text -> Maybe Text
emptyStringToNothing "" = Nothing
emptyStringToNothing txt = Just txt

-- contributors
contributorC :: Contributor.Contributor -> SerializedContributor
contributorC cont =
    SerializedContributor
        (cont ^. Contributor.sign)
        (cont ^. Contributor.name)
        (cont ^. Contributor.email)

contributorsC :: Contributor.Contributors -> [SerializedContributor]
contributorsC conts = contributorC <$> conts'
  where
    conts' = L.sortOn (^. Contributor.sign) $ toList conts

-- tasks
taskContributorsC :: Task.Task -> Taskell.Taskell -> Error.EitherError [Text]
taskContributorsC task tsk = do
    conts <- traverse (`Taskell.getContributor` tsk) (task ^. Task.assigned)
    let symbols = (^. Contributor.sign) <$> conts
    pure $ toList symbols

taskTagsC :: Task.Task -> Taskell.Taskell -> Error.EitherError [Text]
taskTagsC task tsk = do
    tags <- traverse (`Taskell.getTag` tsk) (task ^. Task.tags)
    pure . toList $ (^. Tag.name) <$> tags

taskDescriptionC :: Task.Task -> Maybe Text
taskDescriptionC = emptyStringToNothing . (^. Task.description)

taskTasksC ::
       Task.Task -> RelatedDictionary -> Taskell.Taskell -> Error.EitherError [SerializedTask]
taskTasksC task rel tsk = do
    tasks <- traverse (`Taskell.getTask` tsk) (toList $ task ^. Task.tasks)
    traverse (taskC' rel tsk) tasks

taskRelatedC :: Task.Task -> RelatedDictionary -> Error.EitherError [Related]
taskRelatedC task rel = do
    traverse
        (Error.mEither "Unknown related task" . (`HM.lookup` rel))
        (toList $ task ^. Task.related)

taskC' :: RelatedDictionary -> Taskell.Taskell -> Task.Task -> Error.EitherError SerializedTask
taskC' rel tsk task = do
    let desc = taskDescriptionC task
    conts <- taskContributorsC task tsk
    tags <- taskTagsC task tsk
    tasks <- taskTasksC task rel tsk
    related <- taskRelatedC task rel
    pure $
        emptyTask & taskTitle .~ (task ^. Task.title) & taskDescription .~ desc &
        taskComplete .~ (task ^. Task.complete) &
        taskContributors .~ conts &
        taskTags .~ tags &
        taskRelated .~ related &
        taskTasks .~ tasks

taskC :: RelatedDictionary -> Taskell.Taskell -> Task.TaskID -> Error.EitherError SerializedTask
taskC rel tsk taskID = do
    task <- Taskell.getTask taskID tsk
    taskC' rel tsk task

-- lists
listC :: RelatedDictionary -> Taskell.Taskell -> List.List -> Error.EitherError SerializedList
listC rel tsk lst = do
    tasks <- traverse (taskC rel tsk) (toList (lst ^. List.tasks))
    pure $ SerializedList (lst ^. List.title) tasks

listsC :: RelatedDictionary -> Taskell.Taskell -> Error.EitherError [SerializedList]
listsC rel tsk = do
    lists <- toList <$> Taskell.getLists tsk
    traverse (listC rel tsk) lists

-- related
type RelatedDictionary = HM.HashMap Task.TaskID Related

tasksForList ::
       Taskell.Taskell -> (List.ListID, Text) -> Error.EitherError (Seq (Task.TaskID, (Text, Text)))
tasksForList tsk (lID, title) = do
    tasks <- Taskell.tasksForListWithIDs lID tsk
    let taskTitles = second (^. Task.title) <$> tasks -- [(TaskID 1, "First Task"), ...]
    pure $ second (title, ) <$> taskTitles

merge :: (Text, (Task.TaskID, (Text, Text))) -> (Task.TaskID, Related)
merge (lnk, (tID, (lTitle, tTitle))) = (tID, (lTitle, tTitle, lnk))

relatedDictionary :: Taskell.Taskell -> Error.EitherError RelatedDictionary
relatedDictionary tsk = do
    lists <- Taskell.getListsWithIDs tsk
    let listTitles = second (^. List.title) <$> lists -- [(ListID 1, "First List"), ...]
    titles <- join <$> traverse (tasksForList tsk) listTitles -- [(TaskID 1, ("First List / First Task", "First Task")), ...]
    let links = generateLinks $ snd . snd <$> titles
    let zipped = Seq.zip links titles
    let merged = merge <$> zipped
    pure $ HM.fromList (toList merged)

-- convert
convert :: Taskell.Taskell -> Error.EitherError SerializedTaskell
convert tsk = do
    let title = tsk ^. Taskell.title
    let description = tsk ^. Taskell.description
    let contributors = contributorsC $ tsk ^. Taskell.contributors
    related <- relatedDictionary tsk
    lists <- listsC related tsk
    pure $ SerializedTaskell title description contributors lists
