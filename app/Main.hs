{-# LANGUAGE OverloadedStrings #-}

import Api
import Auth
import Config
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import DB
import Data.Aeson (Value)
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
import Typeclasses
import Types
import Web.Scotty
ret x = json x

routes :: Connection -> ScottyM ()
routes conn = do
    post "/login" $ do
        u <- jsonData :: ActionM (Maybe Credentials)
        let mu = MaybeT . return $ u :: MaybeT IO Credentials
        retStatus <- liftIO $ runMaybeT $ do
          credentials <- mu
          lift $ print credentials
          logUserIn conn credentials
        case retStatus of
          Nothing -> status status401
          Just token -> do
            ret token
            status status200
            
        -- liftIO $ runMaybeT $ do
        --   l <- logUserIn conn u
        --   lift $ status200
    get "/users" $ do
        users <- liftIO $ getUsers conn
        ret users
    put "/users" $ do
        user <- jsonData :: ActionM User
        new <- liftIO $ putUser conn user
        ret new
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        user <- liftIO $ getUserById conn id
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
        teams <- liftIO $ getTeams conn
        ret teams
    put "/teams" $ do
        team <- jsonData :: ActionM Team
        new <- liftIO $ putTeam conn team
        ret new
    get "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        team <- liftIO $ getTeamById conn id
        ret team
    delete "/teams/:id" $ do
        id <- param "id" :: ActionM TL.Text
        liftIO (deleteById conn deleteTeamQuery id)
        ret id
    get "/teams/:id/tasks" $ do
        id <- param "id" :: ActionM TL.Text
        tasks <- liftIO $ getTeamTasks conn id
        ret tasks
    get "/teams/:id/users" $ do
        id <- param "id" :: ActionM TL.Text
        users <- liftIO $ getTeamMembers conn id
        ret users
    put "/teams/:tid/users/add/:uid" $ do
        tid <- param "tid"  :: ActionM TL.Text
        uid <- param "uid"  :: ActionM TL.Text
        liftIO $ putTeamMember conn tid uid
        ret tid
    delete "/teams/:tid/users/delete/:uid" $ do
        tid <- param "tid"  :: ActionM TL.Text
        uid <- param "uid"  :: ActionM TL.Text
        liftIO $ deleteTeamMember conn tid uid
        ret tid

    get "/tasks" $ do
        tasks <- liftIO $ getTasks conn
        ret tasks
    get "/tasks/:id" $ do
        id <- param "id" :: ActionM TL.Text
        task <- liftIO $ getTaskById conn id
        ret task
    put "/tasks" $ do
        task <- jsonData :: ActionM Task
        new <- liftIO $ putTask conn task
        ret new
    get "/tasks/:id/checklist" $ do
        id <- param "id" :: ActionM TL.Text
        checklist <- liftIO $ getTaskChecklist conn id
        ret checklist

    get "/events" $ do
        events <- liftIO $ getEvents conn
        ret events
    get "/events/:id" $ do
        id <- param "id" :: ActionM TL.Text
        event <- liftIO $ getEventById conn id
        ret event
    put "/events" $ do
        event <- jsonData :: ActionM Event
        new <- liftIO $ putEvent conn event
        ret new

    get "/checklists" $ do
        checklists <- liftIO $ getChecklists conn
        ret checklists
    put "/checklists" $ do
        checklist <- jsonData :: ActionM Checklist
        liftIO $ putChecklist conn checklist
        ret checklist

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
    Just (conn, port) -> startServer conn resourcePolicy port routes
    Nothing -> putStrLn "Error: Couldn't create connection" >> return ()
  return ()
