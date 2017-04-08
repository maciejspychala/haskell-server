{-# LANGUAGE OverloadedStrings #-}

module DB where

import Types
import Typeclasses
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
allUsersQuery = "select id, first_name, second_name from users" :: Query
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

teamUsersQuery = "select id, first_name, second_name from users where id in ("
    <> "select userId from user_team where teamId = (?)"
    <> ")" :: Query

-------------- insert queries
insertUserQuery = ("insert into users (first_name, second_name) values (?, ?) returning id" :: Query,
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
insertTeamUserQuery = "insert into user_team (teamId, userId) values (?, ?)" :: Query
deleteTeamUserQuery = "delete from user_team where teamId = (?) and userId = (?)" :: Query
deleteTeamQuery = "delete from teams where id = (?)" :: Query
 
getChecklistByOwner = "select id, task from checklists where task = (?)" :: Query
getUserSalaryById = "select year_salary(?)" :: Query
whereTeam = " where team = (?)" :: Query

-- | returns Just value if there is a head or Nothing when array is empty
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

selectAll :: FromRow q => Connection -> Query -> IO [q]
selectAll = query_

selectById :: FromRow q => Connection -> TL.Text -> Query -> IO (Maybe q)
selectById conn id q = safeHead <$> selectAllBy conn id q

deleteById :: Connection -> Query -> TL.Text -> IO Int64
deleteById conn q id = do
    execute conn q (Only id)

selectAllBy :: FromRow q => Connection -> TL.Text -> Query -> IO [q]
selectAllBy conn id q = query conn q (Only id)

setSalary :: Connection -> User -> IO [Only Int]
setSalary conn user = do
    query conn "select set_salary(?)" $ Only $ getId user

insertInto :: (ToRow r, HasId r) => Connection -> (Query, Query) -> r -> IO r
insertInto conn (insert, update) item = 
    if null $ getId item
        then do
            [Only i] <- query conn insert item
            let newId = fromIntegral (i :: Int64)
                in return $ setId item $ Just newId
        else do
            xs <- execute conn update (toRow item ++ [toField $ getId item])
            return item

insertIdId :: (ToField a) => Connection -> Query -> (a, a) -> IO (a, a)
insertIdId conn insert (id1, id2) = do
    execute conn insert (id1, id2)
    return (id1, id2)

deleteIdId :: (ToField a) => Connection -> Query -> (a, a) -> IO Int64
deleteIdId conn query (id1, id2) = do
    execute conn query (id1, id2)

getWithArray :: (FromRow q, HasArray q) => Connection -> Query -> IO [q]
getWithArray conn query = do
    parents <- selectAll conn query
    mapM (setArray conn) parents


getWithArrayById :: (FromRow q, HasArray q) => Connection -> TL.Text -> Query -> IO (Maybe q)
getWithArrayById conn id query = do
    parent <- selectById conn id query
    case parent of
        Just value -> do
            x <- setArray conn value
            return $ Just x
        Nothing -> return Nothing

getAllWithArrayById :: (Show q, FromRow q, HasArray q) => Connection -> TL.Text -> Query -> IO [q]
getAllWithArrayById conn id query = do
    parents <- selectAllBy conn id query
    mapM (setArray conn) parents

getAllChecklists :: Connection -> IO [Checklist]
getAllChecklists conn = do
    xs <- liftIO $ selectAll conn allChecklistsQuery :: IO [Checklist]
    mapM (setArray conn) xs

insertChecklist :: Connection -> Checklist -> IO ()
insertChecklist conn checklist = do
    let checkId = checklistId checklist
        task = listOwner checklist
        checkWithoutList = Checklist checkId task []
        in liftIO (insertInto conn insertChecklistQuery checkWithoutList)
    mapM_ (insertInto conn insertChecklistItemQuery) $ checklistItems checklist
    return ()
