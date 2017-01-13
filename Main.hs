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


routes :: Connection -> ScottyM ()
routes conn = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name

    get "/users" $ do
        users <- liftIO (selectAll conn getUsersQuery :: IO [User])
        json users
    put "/users" $ do
        user <- jsonData :: ActionM User
        liftIO (insertUser conn user)
        json user
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        user <- liftIO (selectById conn id getUserQueryId :: IO User)
        json user

    get "/teams" $ do
        teams <- liftIO (selectAll conn getTeamsQuery :: IO [Team])
        json teams
    put "/teams" $ do
        team <- jsonData :: ActionM Team
        liftIO (insertTeam conn team)
        json team
    get "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        team <- liftIO (selectById conn id getTeamQueryId :: IO Team)
        json team

    get "/tasks" $ do
        tasks <- liftIO (selectAll conn getTasksQuery :: IO [Task])
        json tasks
    get "/tasks/:id" $ do
        id <- param "id" :: ActionM TL.Text
        task <- liftIO (selectById conn id getTaskQueryId :: IO Task)
        json task


main = do
    x <- readFile "credentials.safe"
    let [username,  password] = words x
    conn <- connectPostgreSQL ("host='95.85.47.237' user='" <> (BS.pack username) <> 
        "' dbname='maciek' password='" <> (BS.pack password) <> "'")
    scotty 3000 (routes conn)
