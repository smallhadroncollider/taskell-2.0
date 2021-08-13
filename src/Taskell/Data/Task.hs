module Taskell.Data.Task
    ( Task(..)
    , Tasks
    , TaskID(..)
    , TaskIDs
    , Parent(..)
    , tasks
    , new
    , removeFromTask
    , belongsToTask
    , rename
    , changeDescription
    , changeCompleted
    , setParentList
    , setParentTask
    , addSubTask
    , addContributor
    , setContributors
    , addTag
    , setTags
    ) where

import RIO
import qualified RIO.Seq as Seq

import Lens.Micro ((<>~))

import Taskell.Data.Types.Contributor (ContributorID)
import Taskell.Data.Types.ID (ContributorIDs, TagIDs)
import Taskell.Data.Types.List (ListID)
import Taskell.Data.Types.Tag (TagID)
import Taskell.Data.Types.Task

new :: Text -> Parent -> Task
new newTitle newParent = Task newTitle newParent "" False [] [] [] []

rename :: Text -> Update
rename newTitle = title .~ newTitle

changeDescription :: Text -> Update
changeDescription newDescription = description .~ newDescription

changeCompleted :: Bool -> Update
changeCompleted cmpl = complete .~ cmpl

-- parent
belongsToTask :: TaskID -> Task -> Bool
belongsToTask taskID task =
    case task ^. parent of
        ParentTask parentID -> taskID == parentID
        _ -> False

setParentList :: ListID -> Update
setParentList listID = parent .~ ParentList listID

setParentTask :: TaskID -> Update
setParentTask taskID = parent .~ ParentTask taskID

-- removing tasks from tasks
removeFromSubTasks :: TaskID -> Update
removeFromSubTasks taskID = tasks %~ Seq.filter (/= taskID)

removeFromRelated :: TaskID -> Update
removeFromRelated taskID = related %~ Seq.filter (/= taskID)

removeFromTask :: TaskID -> Update
removeFromTask taskID = removeFromSubTasks taskID . removeFromRelated taskID

addSubTask :: TaskID -> Update
addSubTask taskID = tasks %~ (Seq.|> taskID)

-- contributors
addContributor :: ContributorID -> Update
addContributor contributorID = assigned <>~ [contributorID]

setContributors :: ContributorIDs -> Update
setContributors contributorIDs = assigned .~ contributorIDs

-- tags
addTag :: TagID -> Update
addTag tagID = tags <>~ [tagID]

setTags :: TagIDs -> Update
setTags tagIDs = tags .~ tagIDs
