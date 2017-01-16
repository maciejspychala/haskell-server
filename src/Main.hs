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
    get "/users" $ do
        users <- liftIO (selectAll conn allUsersQuery :: IO [User])
        ret users
    put "/users" $ do
        user <- jsonData :: ActionM User
        liftIO (insertInto conn insertUserQuery user $ userId user)
        ret user
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        user <- liftIO (selectById conn id getUserQueryById :: IO User)
        ret user

    get "/teams" $ do
        teams <- liftIO (selectAll conn allTeamsQuery :: IO [Team])
        ret teams
    put "/teams" $ do
        team <- jsonData :: ActionM Team
        liftIO (insertInto conn insertTeamQuery team $ teamId team)
        ret team
    get "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        team <- liftIO (selectById conn id getTeamQueryById :: IO Team)
        ret team
    get "/teams/:id/tasks" $ do
        id <- param "id" :: ActionM TL.Text
        tasks <- liftIO (selectAllBy conn id getTasksQueryByTeam :: IO [Task])
        ret tasks

    get "/tasks" $ do
        tasks <- liftIO (selectAll conn allTasksQuery :: IO [Task])
        ret tasks
    get "/tasks/:id" $ do
        id <- param "id" :: ActionM TL.Text
        task <- liftIO (selectById conn id getTaskQueryById :: IO Task)
        ret task
    put "/tasks" $ do
        task <- jsonData :: ActionM Task
        liftIO (insertInto conn insertTaskQuery task $ taskId task)
        ret task

    get "/events" $ do
        events <- liftIO (selectAll conn allEventsQuery :: IO [Event])
        ret events
    get "/events/:id" $ do
        id <- param "id" :: ActionM TL.Text
        event <- liftIO (selectById conn id getEventQueryById :: IO Event)
        ret event
    put "/events" $ do
        event <- jsonData :: ActionM Event
        liftIO (insertInto conn insertEventQuery event $ eventId event)
        ret event

    get "/checklists" $ do
        checklists <- liftIO (getAllChecklists conn)
        ret checklists
    put "/checklists" $ do
        checklist <- jsonData :: ActionM Checklist
        let checkId = checklistId checklist
            task = listOwner checklist
            in do liftIO (insertInto conn insertChecklistQuery [task] checkId)
        mapM (\x -> liftIO $ insertInto conn insertChecklistItemQuery x $ checklistItemId x)
            $ checklistItems checklist
        ret checklist


main = do
    x <- readFile "credentials.safe"
    let [username,  password] = words x
    conn <- connectPostgreSQL ("host='95.85.47.237' user='" <> (BS.pack username) <> 
        "' dbname='maciek' password='" <> (BS.pack password) <> "'")
    scotty 3000 (routes conn)
