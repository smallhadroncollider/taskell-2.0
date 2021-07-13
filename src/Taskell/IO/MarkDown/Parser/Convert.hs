module Taskell.IO.MarkDown.Parser.Convert where

import RIO

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Error as Error

import Taskell.IO.MarkDown.Parser.Types

almostTaskToTaskell ::
       Int -> (NextID, Taskell.Taskell) -> AlmostTask -> Error.EitherError (NextID, Taskell.Taskell)
almostTaskToTaskell listID (ids, tsk) t = do
    let taskID = ids ^. nextTaskID
    updated <- Taskell.addTaskToList (t ^. taskTitle) (Task.TaskID taskID) (List.ListID listID) tsk
    let incremented = ids & nextTaskID %~ (+ 1)
    pure (incremented, updated)

almostListToTaskell ::
       (NextID, Taskell.Taskell) -> AlmostList -> Error.EitherError (NextID, Taskell.Taskell)
almostListToTaskell (ids, tsk) l = do
    let listID = ids ^. nextListID
    updated <- Taskell.addList (l ^. listTitle) (List.ListID listID) tsk
    let incremented = ids & nextListID %~ (+ 1)
    let tasks = l ^. listTasks
    foldM (almostTaskToTaskell listID) (incremented, updated) tasks

almostsToTaskell :: Taskell.Taskell -> [AlmostList] -> Error.EitherError Taskell.Taskell
almostsToTaskell tsk almostLists = snd <$> foldM almostListToTaskell (idStart, tsk) almostLists
