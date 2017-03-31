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
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Exception

ret x = json x

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


getCredentials :: String -> Maybe (String, String, String, String, String, Int)
getCredentials str = return (lines str) >>= \conf -> case conf of
  [a, b, c, d, e, f] -> return (a, b, c, d, e, read f)
  _ -> Nothing

connStringFromCredentials :: (String, String, String, String, String, Int) -> Maybe BS.ByteString
connStringFromCredentials (server, serverPort, dbname, username, password, portString) =
  return $ ("host='" <> (BS.pack server) <> "' port='" <> (BS.pack serverPort)
             <> "' user='" <> (BS.pack username) <> "' dbname='" <> (BS.pack dbname)
             <> "' password='" <> (BS.pack password) <> "'")

createConnection :: BS.ByteString -> Maybe (IO Connection)
createConnection connStr = return $ connectPostgreSQL connStr

startServer :: IO Connection -> CorsResourcePolicy -> Int -> IO ()
startServer ioconn resourcePolicy port = ioconn >>= \conn -> scotty port $ do
  middleware $ cors (const $ Just resourcePolicy)
  routes $ conn

maybeEither e = case e of
  Left _ -> Nothing
  Right m -> Just m

main :: IO ()
main = do
  config <- fmap maybeEither (try $ readFile "credentials.safe" :: IO (Either IOException String))
  
  let resourcePolicy = simpleCorsResourcePolicy {
        corsMethods = ["GET", "POST", "HEAD", "PUT", "DELETE"],
        corsRequestHeaders = ["content-type", "origin"]
        }
        
      connDetails = config >>= getCredentials >>= \c@(_, _, _, _, _, port) ->
        connStringFromCredentials c >>= createConnection >>= (\x -> return (x, port))

  case connDetails of
    Just (conn, port) -> startServer conn resourcePolicy port
    Nothing -> putStrLn "Error: Couldn't create connection" >> return ()
