{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Convert.ToSerialized
    ( convert
    , relatedDictionary
    ) where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.List as L

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

taskRelatedC :: Task.Task -> RelatedDictionary -> Error.EitherError [Text]
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
type RelatedDictionary = HM.HashMap Task.TaskID Text

relatedDictionary :: Taskell.Taskell -> Error.EitherError RelatedDictionary
relatedDictionary tsk = do
    let listIDs = tsk ^. Taskell.listsOrder
    tasks <- traverse (`Taskell.tasksForListWithIDs` tsk) listIDs
    let titles = generateLinks $ second (^. Task.title) <$> join tasks
    pure $ HM.fromList (toList titles)

-- convert
convert :: Taskell.Taskell -> Error.EitherError SerializedTaskell
convert tsk = do
    let title = tsk ^. Taskell.title
    let description = tsk ^. Taskell.description
    let contributors = contributorsC $ tsk ^. Taskell.contributors
    related <- relatedDictionary tsk
    lists <- listsC related tsk
    pure $ SerializedTaskell title description contributors lists
