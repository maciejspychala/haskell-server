module Api.Tasks
where

import DB
import Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Types

getTasks :: Connection -> IO [Task]
getTasks conn = selectAll conn allTasksQuery

getTaskById :: Connection -> TL.Text -> IO (Maybe Task)
getTaskById conn id = selectById conn id getTaskQueryById

putTask :: Connection -> Task -> IO Task
putTask conn task = insertInto conn insertTaskQuery task

getTaskChecklist :: Connection -> TL.Text -> IO (Maybe Checklist)
getTaskChecklist conn id = getWithArrayById conn id getChecklistByOwner
