{-# LANGUAGE TemplateHaskell #-}

module Taskell.IO.MarkDown.Parser.Convert
    ( convert
    ) where

import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Seq as Seq

import Lens.Micro ((+~))
import Lens.Micro.TH (makeLenses)

import qualified Taskell.Data.Taskell as Taskell
import qualified Taskell.Data.Types.Contributor as Contributor
import qualified Taskell.Data.Types.List as List
import qualified Taskell.Data.Types.Task as Task
import qualified Taskell.Data.Types.Taskell as Taskell
import qualified Taskell.Error as Error

import Taskell.IO.MarkDown.Parser.Document (parse)
import Taskell.IO.MarkDown.Parser.Types
import Taskell.IO.MarkDown.Types

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
parsedTaskToTaskell ::
       List.ListID
    -> (NextID, Taskell.Taskell)
    -> ParsedTask
    -> Error.EitherError (NextID, Taskell.Taskell)
parsedTaskToTaskell listID (ids, tsk) t = do
    let taskID = Task.TaskID (ids ^. nextTaskID)
    updated <- Taskell.addTaskToList (t ^. taskTitle) taskID listID tsk
    let conts =
            Seq.fromList . catMaybes $
            Taskell.findContributorFromSign tsk <$> (t ^. taskContributors)
    updated' <- Taskell.setTaskContributors conts taskID updated
    pure (incTask ids, updated')

parsedListToTaskell ::
       (NextID, Taskell.Taskell) -> ParsedList -> Error.EitherError (NextID, Taskell.Taskell)
parsedListToTaskell (ids, tsk) l = do
    let listID = List.ListID (ids ^. nextListID)
    updated <- Taskell.addList (l ^. listTitle) listID tsk
    foldM (parsedTaskToTaskell listID) (incList ids, updated) (l ^. listTasks)

parsedListsToTaskell :: Taskell.Taskell -> [ParsedList] -> Error.EitherError Taskell.Taskell
parsedListsToTaskell tsk parsedLists = snd <$> foldM parsedListToTaskell (idStart, tsk) parsedLists

parsedContributorToContributor :: ParsedContributor -> Contributor.Contributor
parsedContributorToContributor parsed =
    Contributor.Contributor
        (parsed ^. contributorSign)
        (parsed ^. contributorName)
        (parsed ^. contributorEmail)

contributors :: [ParsedContributor] -> Contributor.Contributors
contributors parsed = HM.fromList (zip ids cs)
  where
    cs = parsedContributorToContributor <$> parsed
    ids = Contributor.ContributorID <$> [1 ..]

parsedToTaskell :: ParsedTaskell -> Error.EitherError Taskell.Taskell
parsedToTaskell parsed = do
    let tsk =
            Taskell.create (parsed ^. taskellTitle) &
            Taskell.description .~ (parsed ^. taskellDescription) &
            Taskell.contributors .~ contributors (parsed ^. taskellContributors)
    parsedListsToTaskell tsk (parsed ^. taskellLists)

convert :: Dictionary -> Text -> Error.EitherError Taskell.Taskell
convert dictionary input = either Error.e parsedToTaskell $ parse dictionary input
