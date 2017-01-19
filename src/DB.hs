{-# LANGUAGE OverloadedStrings #-}

module DB where

import Typeclasses
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



-------------- select all
allUsersQuery = "select id, first_name, second_name, team from users" :: Query
allTeamsQuery = "select id, name from teams" :: Query
allTasksQuery = "select id, begin_date at time zone 'utc', end_date at time zone 'utc', team, description from tasks" :: Query
allEventsQuery = "select id, name, creator from events" :: Query
allChecklistsQuery = "select id, task from checklists" :: Query

-------------- select by Id
whereId = " where id = (?)" :: Query
getUserQueryById = allUsersQuery <> whereId
getTaskQueryById = allTasksQuery <> whereId 
getTeamQueryById = allTeamsQuery <> whereId
getEventQueryById = allEventsQuery <> whereId


-------------- insert queries
insertUserQuery = ("insert into users (first_name, second_name, team) values (?, ?, ?) returning id" :: Query,
    "update users set first_name = (?), second_name = (?), team = (?) where id = (?)" :: Query)
insertTeamQuery = ("insert into teams (name) values (?) returning id" :: Query,
    "update teams set name = (?) where id = (?)" :: Query)
insertTaskQuery = ("insert into tasks (begin_date, end_date, team, description) values (?, ?, ?, ?) returning id" :: Query,
    "update tasks set begin_date = (?), end_date = (?), team = (?), description = (?) where id = (?)" :: Query)
insertEventQuery = ("insert into events (name, creator) values (?, ?) returning id" :: Query,
    "update events set name = (?), creator = (?) where id = (?)" :: Query)
insertChecklistQuery = ("insert into checklists (task) values (?) returning id" :: Query,
    "update checklists set task = (?) where id = (?)" :: Query)
insertChecklistItemQuery = ("insert into checklistitems (name, finished, checklist) values (?, ?, ?) returning id" :: Query,
    "update checklistitems set name = (?), finished = (?), checklist = (?) where id = (?)" :: Query)



getChecklistsItemQueryByTeamId = "select id, name, finished, checklist from checklistitems where checklist = (?)" :: Query

whereTeam = " where team = (?)" :: Query

selectAll :: FromRow q => Connection -> Query -> IO [q]
selectAll conn q = do
    allRows <- query_ conn q
    return allRows
    


selectById :: FromRow q => Connection -> TL.Text -> Query -> IO q
selectById conn id q = do
    tableWithOneRow <- query conn q (Only id)
    return (head tableWithOneRow)

selectAllBy :: FromRow q => Connection -> TL.Text -> Query -> IO [q]
selectAllBy conn id q = do
    xs <- query conn q (Only id)
    return xs


insertInto :: (ToRow r, HasId r) => Connection -> (Query, Query) -> r -> IO r
insertInto conn (insert, update) item = do
    if null $ getId item
        then do
            [Only i] <- (query conn insert item)
            let newId = fromIntegral (i :: Int64)
                in return $ setId item $ Just newId
        else do
            xs <- execute conn update (toRow item ++ [toField $ getId item])
            return $ item

getWithArray :: (FromRow q, HasArray q) => Connection -> Query -> IO [q]
getWithArray conn query = do
    parents <- selectAll conn query
    mapM (\x -> setArray conn x) parents

getAllChecklists :: Connection -> IO [Checklist]
getAllChecklists conn = do
    xs <- liftIO $ query_ conn allChecklistsQuery :: IO [Checklist]
    mapM (\x -> setArray conn x) xs

insertChecklist :: Connection -> Checklist -> IO ()
insertChecklist conn checklist = do
    let checkId = checklistId checklist
        task = listOwner checklist
        checkWithoutList = Checklist checkId task []
        in do liftIO (insertInto conn insertChecklistQuery checkWithoutList)
    mapM (\x -> liftIO $ insertInto conn insertChecklistItemQuery x)
        $ checklistItems checklist
    return ()
