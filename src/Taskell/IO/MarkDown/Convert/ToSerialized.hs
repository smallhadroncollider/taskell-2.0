{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Convert.ToSerialized
    ( convert
    ) where

import RIO
import qualified RIO.List as L

import qualified Taskell.Error as Error

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Tag as Tag
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Data.Types.Taskell as Taskell

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

taskTasksC :: Task.Task -> Taskell.Taskell -> Error.EitherError [SerializedTask]
taskTasksC task tsk = do
    tasks <- traverse (`Taskell.getTask` tsk) (toList $ task ^. Task.tasks)
    traverse (taskC' tsk) tasks

taskC' :: Taskell.Taskell -> Task.Task -> Error.EitherError SerializedTask
taskC' tsk task = do
    let desc = taskDescriptionC task
    conts <- taskContributorsC task tsk
    tags <- taskTagsC task tsk
    tasks <- taskTasksC task tsk
    pure $
        emptyTask & taskTitle .~ (task ^. Task.title) & taskDescription .~ desc &
        taskContributors .~ conts &
        taskTags .~ tags &
        taskTasks .~ tasks

taskC :: Taskell.Taskell -> Task.TaskID -> Error.EitherError SerializedTask
taskC tsk taskID = do
    task <- Taskell.getTask taskID tsk
    taskC' tsk task

-- lists
listC :: Taskell.Taskell -> List.List -> Error.EitherError SerializedList
listC tsk lst = do
    tasks <- traverse (taskC tsk) (toList (lst ^. List.tasks))
    pure $ SerializedList (lst ^. List.title) tasks

listsC :: Taskell.Taskell -> Error.EitherError [SerializedList]
listsC tsk = do
    lists <- toList <$> Taskell.getLists tsk
    traverse (listC tsk) lists

-- convert
convert :: Taskell.Taskell -> Error.EitherError SerializedTaskell
convert tsk = do
    let title = tsk ^. Taskell.title
    let description = tsk ^. Taskell.description
    let contributors = contributorsC $ tsk ^. Taskell.contributors
    lists <- listsC tsk
    pure $ SerializedTaskell title description contributors lists
