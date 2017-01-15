{-# LANGUAGE OverloadedStrings #-}

module DB where

import Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Web.Scotty
import Data.Monoid ((<>))
import Control.Monad.IO.Class
import Data.Int

getUsersQuery = "select id, first_name, second_name, team from users" :: Query
getTeamsQuery = "select id, name from teams" :: Query
getTasksQuery = "select id, begin_date at time zone 'utc', end_date at time zone 'utc', team, description from tasks" :: Query
getEventsQuery = "select id, name, creator from events" :: Query
getChecklistsQuery = "select id, task from checklists" :: Query
getChecklistsItemQueryByTeamId = "select id, name, finished, checklist from checklistitems where checklist = (?)" :: Query

getUserQueryId = "select id, first_name, second_name, team from users where id = (?)" :: Query
getTaskQueryId = "select id, begin_date at time zone 'utc', end_date at time zone 'utc', team, description from tasks where id = (?)" :: Query
getTeamQueryId = "select id, name from teams where id = (?)" :: Query
getEventQueryId = "select id, name, creator from events where id = (?)" :: Query

insertUserQuery = ("insert into users (first_name, second_name, team) values (?, ?, ?)" :: Query,
    "update users set first_name = (?), second_name = (?), team = (?) where id = (?)" :: Query)

insertTeamQuery = ("insert into teams (name) values (?)" :: Query,
    "update teams set name = (?) where id = (?)" :: Query)

insertTaskQuery = ("insert into tasks (begin_date, end_date, team, description) values (?, ?, ?, ?)" :: Query,
    "update tasks set begin_date = (?), end_date = (?), team = (?), description = (?) where id = (?)" :: Query)
insertEventQuery = ("insert into events (name, creator) values (?, ?)" :: Query,
    "update events set name = (?), creator = (?) where id = (?)" :: Query)

hello :: ActionM ()
hello = do
    text "hello world!"

helloName :: TL.Text -> ActionM ()
helloName name = do
    text ("hello " <> name <> " :*")


selectAll :: FromRow q => Connection -> Query -> IO [q]
selectAll conn q = do
    allRows <- query_ conn q
    return allRows
    

selectById :: FromRow q => Connection -> TL.Text -> Query -> IO q
selectById conn id q = do
    tableWithOneRow <- query conn q (Only id)
    return (head tableWithOneRow)

insertInto :: ToRow r => Connection -> (Query, Query) -> r -> Maybe Int -> IO Int64
insertInto conn (update, insert) item id = do
    if null $ id
        then execute conn update item
        else execute conn insert (toRow item ++ [toField $ id])

getChecklists :: Connection -> IO [Checklist]
getChecklists conn = do
    xs <- liftIO $ query_ conn getChecklistsQuery :: IO [(Maybe Int, Int)]
    mapM (\x -> makeChecklist conn x) xs

makeChecklist :: Connection -> (Maybe Int, Int) -> IO Checklist
makeChecklist conn (checkId, task) = do
    items <- getChecklistsItems conn task
    return $ Checklist checkId task items

getChecklistsItems :: Connection -> Int -> IO [ChecklistItem]
getChecklistsItems conn checkId = do
    items <- query conn getChecklistsItemQueryByTeamId (Only checkId)
    return items
