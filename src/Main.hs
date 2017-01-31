{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Typeclasses
import Types
import DB
import Network.Wai.Middleware.Cors

ret x = do
    json x
    

routes :: Connection -> ScottyM ()
routes conn = do
    get "/users" $ do
        users <- liftIO (getWithArray conn allUsersQuery :: IO [User])
        ret users
    put "/users" $ do
        user <- jsonData :: ActionM User
        new <- liftIO (insertInto conn insertUserQuery user)
        liftIO (setSalary conn new)
        ret new
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        user <- liftIO (getWithArrayById conn id getUserQueryById :: IO (Maybe User))
        ret user
    get "/users/:id/salary" $ do
        id <- param "id" :: ActionM TL.Text
        x <- liftIO (selectById conn id getUserSalaryById :: IO (Maybe (Only Double)))
        case x of
            Nothing -> do
                ret (Nothing :: Maybe Salary)
            Just value -> do
                ret $ Salary (read $ TL.unpack id :: Int) $ fromOnly value

    get "/teams" $ do
        teams <- liftIO (selectAll conn allTeamsQuery :: IO [Team])
        ret teams
    put "/teams" $ do
        team <- jsonData :: ActionM Team
        new <- liftIO (insertInto conn insertTeamQuery team)
        ret new
    get "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        team <- liftIO (selectById conn id getTeamQueryById :: IO (Maybe Team))
        ret team
    delete "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        liftIO (deleteById conn deleteTeamQuery id)
        ret id
    get "/teams/:id/tasks" $ do
        id <- param "id" :: ActionM TL.Text
        tasks <- liftIO (selectAllBy conn id (allTasksQuery <> whereTeam) :: IO [Task])
        ret tasks
    get "/teams/:id/users" $ do
        id <- param "id" :: ActionM TL.Text
        users <- liftIO (getAllWithArrayById conn id teamUsersQuery :: IO [User])
        ret users
    put "/teams/:tid/users/add/:uid" $ do
        tid <- param "tid"  :: ActionM TL.Text
        uid <- param "uid"  :: ActionM TL.Text
        liftIO (insertIdId conn insertTeamUserQuery (tid, uid))
        ret tid
    delete "/teams/:tid/users/delete/:uid" $ do
        tid <- param "tid"  :: ActionM TL.Text
        uid <- param "uid"  :: ActionM TL.Text
        liftIO (deleteIdId conn deleteTeamUserQuery (tid, uid))
        ret tid

    get "/tasks" $ do
        tasks <- liftIO (selectAll conn allTasksQuery :: IO [Task])
        ret tasks
    get "/tasks/:id" $ do
        id <- param "id" :: ActionM TL.Text
        task <- liftIO (selectById conn id getTaskQueryById :: IO (Maybe Task))
        ret task
    put "/tasks" $ do
        task <- jsonData :: ActionM Task
        new <- liftIO (insertInto conn insertTaskQuery task)
        ret new
    get "/tasks/:id/checklist" $ do
        id <- param "id" :: ActionM TL.Text
        checklist <- liftIO (getWithArrayById conn id getChecklistByOwner :: IO (Maybe Checklist))
        ret checklist

    get "/events" $ do
        events <- liftIO (selectAll conn allEventsQuery :: IO [Event])
        ret events
    get "/events/:id" $ do
        id <- param "id" :: ActionM TL.Text
        event <- liftIO (selectById conn id getEventQueryById :: IO (Maybe Event))
        ret event
    put "/events" $ do
        event <- jsonData :: ActionM Event
        new <- liftIO (insertInto conn insertEventQuery event)
        ret new

    get "/checklists" $ do
        checklists <- liftIO (getWithArray conn allChecklistsQuery :: IO [Checklist])
        ret checklists
    put "/checklists" $ do
        checklist <- jsonData :: ActionM Checklist
        liftIO $ insertChecklist conn checklist
        ret checklist



main = do
    x <- readFile "credentials.safe"
    let [username,  password, portString] = words x
        port = read portString :: Int
    let resourcePolicy = simpleCorsResourcePolicy { corsMethods = ["GET", "POST", "HEAD", "PUT", "DELETE"], corsRequestHeaders = ["content-type", "origin"] } 
    conn <- connectPostgreSQL ("host='95.85.47.237' user='" <> (BS.pack username) <> 
        "' dbname='maciek' password='" <> (BS.pack password) <> "'")
    scotty port $ do
        middleware $ cors (const $ Just resourcePolicy)
        routes conn
