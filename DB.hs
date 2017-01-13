{-# LANGUAGE OverloadedStrings #-}

module DB where

import Types
import Database.PostgreSQL.Simple
import qualified Data.Text.Lazy as TL
import Web.Scotty
import Data.Monoid ((<>))
import Data.Int

getUsersQuery = "select id, first_name, second_name, team from users" :: Query
getTeamsQuery = "select id, name from teams" :: Query
getTasksQuery = "select id, begin_date at time zone 'utc', end_date at time zone 'utc', team, description from tasks" :: Query

getUserQueryId = "select id, first_name, second_name, team from users where id = (?)" :: Query
getTaskQueryId = "select id, begin_date at time zone 'utc', end_date at time zone 'utc', team, description from tasks where id = (?)" :: Query
getTeamQueryId = "select id, name from teams where id = (?)" :: Query

insertUserQuery = "insert into users (first_name, second_name, team) values (?, ?, ?)" :: Query
updateUserQuery = "update users set first_name = (?), second_name = (?), team = (?) where id = (?)" :: Query

insertTeamQuery = "insert into teams (name) values (?)" :: Query
updateTeamQuery = "update teams set name = (?) where id = (?)" :: Query



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


insertUser :: Connection -> User -> IO Int64
insertUser conn user = do
    if null $ userId user 
        then execute conn insertUserQuery user
        else execute conn updateUserQuery (firstName user, lastName user, team user, userId user)

insertTeam :: Connection -> Team -> IO Int64
insertTeam conn team = do
    if null $ teamId team 
        then execute conn insertTeamQuery team
        else execute conn updateTeamQuery (name team, teamId team)

