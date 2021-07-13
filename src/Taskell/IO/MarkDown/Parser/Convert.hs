{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Parser.Convert where

import RIO
import qualified RIO.Seq as Seq

import Lens.Micro ((+~))
import Lens.Micro.TH (makeLenses)

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Error as Error

import Taskell.IO.MarkDown.Parser.Types

-- ID tracking
data NextID =
    NextID
        { _nextTaskID :: !Int
        , _nextListID :: !Int
        }

makeLenses ''NextID

incList :: NextID -> NextID
incList = nextListID +~ 1

incTask :: NextID -> NextID
incTask = nextTaskID +~ 1

idStart :: NextID
idStart = NextID 1 1

-- conversion
almostTaskToTaskell ::
       List.ListID
    -> (NextID, Taskell.Taskell)
    -> AlmostTask
    -> Error.EitherError (NextID, Taskell.Taskell)
almostTaskToTaskell listID (ids, tsk) t = do
    let taskID = Task.TaskID (ids ^. nextTaskID)
    updated <- Taskell.addTaskToList (t ^. taskTitle) taskID listID tsk
    let contributors =
            Seq.fromList . catMaybes $
            Taskell.findContributorFromSign tsk <$> (t ^. taskContributors)
    updated' <- Taskell.setTaskContributors contributors taskID updated
    pure (incTask ids, updated')

almostListToTaskell ::
       (NextID, Taskell.Taskell) -> AlmostList -> Error.EitherError (NextID, Taskell.Taskell)
almostListToTaskell (ids, tsk) l = do
    let listID = List.ListID (ids ^. nextListID)
    updated <- Taskell.addList (l ^. listTitle) listID tsk
    foldM (almostTaskToTaskell listID) (incList ids, updated) (l ^. listTasks)

almostsToTaskell :: Taskell.Taskell -> [AlmostList] -> Error.EitherError Taskell.Taskell
almostsToTaskell tsk almostLists = snd <$> foldM almostListToTaskell (idStart, tsk) almostLists
