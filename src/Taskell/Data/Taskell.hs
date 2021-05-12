module Taskell.Data.Taskell
    ( TT.Taskell(..)
    , e
    , removeTasks
    , tasksForList
    , tasksForTask
    , getLists
    , moveListLeft
    , moveListRight
    , renameList
    , rename
    , changeDescription
    , removeList
    , addList
    , addTaskToList
    , renameTask
    , changeTaskDescription
    , moveTaskUp
    , moveTaskDown
    , moveTaskLeft
    , moveTaskRight
    , moveTaskLeftTop
    , moveTaskRightTop
    ) where

import RIO
import qualified RIO.HashMap as HM (adjust, delete, insert, lookup)

import qualified Taskell.Data.List as TTL
import qualified Taskell.Data.Task as TTT
import qualified Taskell.Data.Types.ID as ID
import qualified Taskell.Data.Types.List as TTL
import qualified Taskell.Data.Types.Task as TTT
import qualified Taskell.Data.Types.Taskell as TT

newtype Error =
    Error Text
    deriving (Show, Eq)

e :: Text -> Either Error a
e text = Left (Error text)

mEither :: Text -> Maybe a -> Either Error a
mEither text = maybe (e text) pure

type Result a = Either Error a

type Update = TT.Taskell -> Result TT.Taskell

-- getting lists
getList :: TTL.ListID -> TT.Taskell -> Result TTL.List
getList listID taskell =
    mEither ("Unknown reference: " <> tshow listID) (HM.lookup listID (taskell ^. TT.lists))

getLists :: TT.Taskell -> [TTL.List]
getLists taskell =
    fromMaybe [] $ traverse (`HM.lookup` (taskell ^. TT.lists)) (taskell ^. TT.listsOrder)

-- reordering lists
moveListLeft :: TTL.ListID -> Update
moveListLeft listID taskell = pure (taskell & TT.listsOrder %~ ID.moveLeft listID)

moveListRight :: TTL.ListID -> Update
moveListRight listID taskell = pure (taskell & TT.listsOrder %~ ID.moveRight listID)

-- working with lists
updateList :: TTL.Update -> TTL.ListID -> Update
updateList fn listID taskell = do
    list <- fn <$> getList listID taskell
    pure (taskell & TT.lists %~ HM.insert listID list)

renameList :: Text -> TTL.ListID -> Update
renameList title = updateList (TTL.rename title)

addList :: Text -> TTL.ListID -> Update
addList title listID taskell =
    pure (taskell & TT.lists %~ HM.insert listID (TTL.new title) & TT.listsOrder %~ (<> [listID]))

removeList :: TTL.ListID -> Update
removeList listID taskell =
    pure (taskell & TT.lists %~ HM.delete listID & TT.listsOrder %~ filter (/= listID))

getListLeft :: TTL.ListID -> TT.Taskell -> Maybe TTL.ListID
getListLeft listID taskell = do
    let list = taskell ^. TT.listsOrder
    ID.getToLeft listID list

getListRight :: TTL.ListID -> TT.Taskell -> Maybe TTL.ListID
getListRight listID taskell = do
    let list = taskell ^. TT.listsOrder
    ID.getToRight listID list

-- getting tasks
updateTask :: TTT.Update -> TTT.TaskID -> Update
updateTask fn taskID taskell = do
    task <- fn <$> getTask taskID taskell
    pure (taskell & TT.tasks %~ HM.insert taskID task)

taskIDsToTasks :: TT.Taskell -> TTT.TaskIDs -> Result [TTT.Task]
taskIDsToTasks taskell = traverse (`getTask` taskell)

tasksForList :: TTL.ListID -> TT.Taskell -> Result [TTT.Task]
tasksForList listID taskell = getList listID taskell >>= taskIDsToTasks taskell . (^. TTL.tasks)

tasksForTask :: TTT.TaskID -> TT.Taskell -> Result [TTT.Task]
tasksForTask taskID taskell = getTask taskID taskell >>= taskIDsToTasks taskell . (^. TTT.tasks)

getTask :: TTT.TaskID -> TT.Taskell -> Either Error TTT.Task
getTask taskID taskell =
    mEither ("Unknown reference: " <> tshow taskID) (HM.lookup taskID (taskell ^. TT.tasks))

addTaskToList :: Text -> TTT.TaskID -> TTL.ListID -> Update
addTaskToList title taskID listID taskell = do
    updated <- updateList (TTL.addTask taskID) listID taskell
    pure (updated & TT.tasks %~ HM.insert taskID (TTT.new title (TTT.ParentList listID)))

renameTask :: Text -> TTT.TaskID -> Update
renameTask title = updateTask (TTT.rename title)

changeTaskDescription :: Text -> TTT.TaskID -> Update
changeTaskDescription title = updateTask (TTT.changeDescription title)

moveTaskUp :: TTT.TaskID -> Update
moveTaskUp taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentTask parentID -> updateTask (TTT.moveUp taskID) parentID taskell
        TTT.ParentList parentID -> updateList (TTT.moveUp taskID) parentID taskell

moveTaskDown :: TTT.TaskID -> Update
moveTaskDown taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentTask parentID -> updateTask (TTT.moveDown taskID) parentID taskell
        TTT.ParentList parentID -> updateList (TTT.moveDown taskID) parentID taskell

moveTaskLR ::
       (TTL.ListID -> TT.Taskell -> Maybe TTL.ListID)
    -> (TTT.TaskID -> TTL.List -> TTL.List)
    -> TTT.TaskID
    -> Update
moveTaskLR getListLR addTaskTB taskID taskell = do
    task <- getTask taskID taskell
    case task ^. TTT.parent of
        TTT.ParentTask _ -> pure taskell
        TTT.ParentList currentListID -> do
            case getListLR currentListID taskell of
                Nothing -> pure taskell
                Just intoID -> do
                    let updatedTask = task & TTT.parent .~ TTT.ParentList intoID
                    let lists = taskell ^. TT.lists
                    let removed = HM.adjust (TTL.removeFromList taskID) currentListID lists
                    let added = HM.adjust (addTaskTB taskID) intoID removed
                    pure (taskell & TT.lists .~ added & TT.tasks %~ HM.insert taskID updatedTask)

moveTaskLeft :: TTT.TaskID -> Update
moveTaskLeft = moveTaskLR getListLeft TTL.addTask

moveTaskRight :: TTT.TaskID -> Update
moveTaskRight = moveTaskLR getListRight TTL.addTask

moveTaskLeftTop :: TTT.TaskID -> Update
moveTaskLeftTop = moveTaskLR getListLeft TTL.addTaskTop

moveTaskRightTop :: TTT.TaskID -> Update
moveTaskRightTop = moveTaskLR getListRight TTL.addTaskTop

-- removing tasks
removeChildren :: TTT.TaskID -> Update
removeChildren taskID taskell = do
    task <- getTask taskID taskell
    foldM (flip removeFromTasks) taskell (task ^. TTT.tasks)

removeFromTasks :: TTT.TaskID -> Update
removeFromTasks taskID taskell = do
    removedChildren <- removeChildren taskID taskell
    let removed = HM.delete taskID (removedChildren ^. TT.tasks)
    let removedImmeditate = TTT.removeFromTask taskID <$> removed
    pure (taskell & TT.tasks .~ removedImmeditate)

removeFromLists :: TTT.TaskID -> Update
removeFromLists taskID taskell = do
    task <- getTask taskID taskell
    pure $
        case task ^. TTT.parent of
            TTT.ParentList listID ->
                (taskell & TT.lists %~ HM.adjust (TTL.removeFromList taskID) listID)
            _ -> taskell

removeTasks :: TTT.TaskID -> Update
removeTasks taskID taskell = removeFromLists taskID taskell >>= removeFromTasks taskID

-- Taskell
rename :: Text -> Update
rename title taskell = pure (taskell & TT.title .~ title)

changeDescription :: Text -> Update
changeDescription title taskell = pure (taskell & TT.description .~ title)
