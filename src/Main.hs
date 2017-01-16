{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Types
import DB


ret x = do
    addHeader "Access-Control-Allow-Origin" "*"
    json x
    

routes :: Connection -> ScottyM ()
routes conn = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name

    get "/users" $ do
        users <- liftIO (selectAll conn getUsersQuery :: IO [User])
        ret users
    put "/users" $ do
        user <- jsonData :: ActionM User
        liftIO (insertInto conn insertUserQuery user $ userId user)
        ret user
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        user <- liftIO (selectById conn id getUserQueryId :: IO User)
        ret user

    get "/teams" $ do
        teams <- liftIO (selectAll conn getTeamsQuery :: IO [Team])
        ret teams
    put "/teams" $ do
        team <- jsonData :: ActionM Team
        liftIO (insertInto conn insertTeamQuery team $ teamId team)
        ret team
    get "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        team <- liftIO (selectById conn id getTeamQueryId :: IO Team)
        ret team
    get "/teams/:id/tasks" $ do
        id <- param "id" :: ActionM TL.Text
        tasks <- liftIO (selectBy conn id getTasksQueryByTeam :: IO [Task])
        ret tasks

    get "/tasks" $ do
        tasks <- liftIO (selectAll conn getTasksQuery :: IO [Task])
        ret tasks
    get "/tasks/:id" $ do
        id <- param "id" :: ActionM TL.Text
        task <- liftIO (selectById conn id getTaskQueryId :: IO Task)
        ret task
    put "/tasks" $ do
        task <- jsonData :: ActionM Task
        liftIO (insertInto conn insertTaskQuery task $ taskId task)
        ret task

    get "/events" $ do
        events <- liftIO (selectAll conn getEventsQuery :: IO [Event])
        ret events
    get "/events/:id" $ do
        id <- param "id" :: ActionM TL.Text
        event <- liftIO (selectById conn id getEventQueryId :: IO Event)
        ret event
    put "/events" $ do
        event <- jsonData :: ActionM Event
        liftIO (insertInto conn insertEventQuery event $ eventId event)
        ret event

    get "/checklists" $ do
        checklists <- liftIO (getChecklists conn)
        ret checklists


main = do
    x <- readFile "credentials.safe"
    let [username,  password] = words x
    conn <- connectPostgreSQL ("host='95.85.47.237' user='" <> (BS.pack username) <> 
        "' dbname='maciek' password='" <> (BS.pack password) <> "'")
    scotty 80 (routes conn)
