{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Monoid ((<>))
import Control.Monad.IO.Class
import Data.Int
import SqlTypes
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as IBS

getUsersQuery = "select id, first_name, second_name, team from users"
getTeamsQuery = "select id, name from teams"
getUserQueryId = "select id, first_name, second_name, team from users where id = (?)"
insertUserQuery = "insert into users (first_name, second_name, team) values (?, ?, ?)"
updateUserQuery = "update users set first_name = (?), second_name = (?), team = (?) where id = (?)"
insertTeamQuery = "insert into teams (name) values (?)"
updateTeamQuery = "update teams set name = (?) where id = (?)"

routes :: Connection -> ScottyM ()
routes conn = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name
    get "/users" $ do
        users <- liftIO (getUsers conn)
        json users
    put "/users" $ do
        user <- jsonData :: ActionM User
        liftIO (insertUser conn user)
        json user
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        user <- liftIO (getUser conn id)
        json user
    get "/teams" $ do
        teams <- liftIO (getTeams conn)
        json teams
    put "/teams" $ do
        team <- jsonData :: ActionM Team
        liftIO (insertTeam conn team)
        json team


hello :: ActionM ()
hello = do
    text "hello world!"

helloName :: TL.Text -> ActionM ()
helloName name = do
    text ("hello " <> name <> " :*")

getUsers :: Connection -> IO [User]
getUsers conn = do
    users <- query_ conn getUsersQuery :: IO [User]
    return users

getTeams :: Connection -> IO [Team]
getTeams conn = do
    teams <- query_ conn getTeamsQuery :: IO [Team]
    return teams

getUser :: Connection -> TL.Text -> IO User
getUser conn id = do
    users <- query conn getUserQueryId (Only id) :: IO [User]
    return (head users)

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

main = do
    x <- readFile "credentials.safe"
    let [username,  password] = words x
    conn <- connectPostgreSQL ("host='95.85.47.237' user='" <> (IBS.pack username) <> 
        "' dbname='maciek' password='" <> (IBS.pack password) <> "'")
    scotty 3000 (routes conn)
