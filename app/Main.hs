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
import Data.Either
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Config

ret x = json x

routes :: Reader Config (ScottyM ())
routes = do
    conn <- fmap fromJust $ asks connection
    return $ do
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
                    liftIO $ print token
                    ret token
                    status status200
        get "/users" $ authorize $ do
            users <- liftIO $ getUsers conn
            ret users
        put "/users" $ do
            user <- jsonData :: ActionM User
            new <- liftIO $ putUser conn user
            ret new
        get "/users/:id" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            user <- liftIO $ getUserById conn id
            ret user
        get "/users/:id/salary" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            x <- liftIO (selectById conn id getUserSalaryById :: IO (Maybe (Only Double)))
            case x of
                Nothing -> do
                    ret (Nothing :: Maybe Salary)
                Just value -> do
                    ret $ Salary (read $ TL.unpack id :: Int) $ fromOnly value

        get "/teams" $ authorize $ do
            teams <- liftIO $ getTeams conn
            ret teams
        put "/teams" $ do
            team <- jsonData :: ActionM Team
            new <- liftIO $ putTeam conn team
            ret new
        get "/teams/:id" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            team <- liftIO $ getTeamById conn id
            ret team
        delete "/teams/:id" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            liftIO (deleteById conn deleteTeamQuery id)
            ret id
        get "/teams/:id/tasks" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            tasks <- liftIO $ getTeamTasks conn id
            ret tasks
        get "/teams/:id/users" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            users <- liftIO $ getTeamMembers conn id
            ret users
        put "/teams/:tid/users/add/:uid" $ authorize $ do
            tid <- param "tid"  :: ActionM TL.Text
            uid <- param "uid"  :: ActionM TL.Text
            liftIO $ putTeamMember conn tid uid
            ret tid
        delete "/teams/:tid/users/delete/:uid" $ authorize $ do
            tid <- param "tid"  :: ActionM TL.Text
            uid <- param "uid"  :: ActionM TL.Text
            liftIO $ deleteTeamMember conn tid uid
            ret tid

        get "/tasks" $ authorize $ do
            tasks <- liftIO $ getTasks conn
            ret tasks
        get "/tasks/:id" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            task <- liftIO $ getTaskById conn id
            ret task
        put "/tasks" $ authorize $ do
            task <- jsonData :: ActionM Task
            new <- liftIO $ putTask conn task
            ret new
        get "/tasks/:id/checklist" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            checklist <- liftIO $ getTaskChecklist conn id
            ret checklist

        get "/events" $ authorize $ do
            events <- liftIO $ getEvents conn
            ret events
        get "/events/:id" $ authorize $ do
            id <- param "id" :: ActionM TL.Text
            event <- liftIO $ getEventById conn id
            ret event
        put "/events" $ authorize $ do
            event <- jsonData :: ActionM Event
            new <- liftIO $ putEvent conn event
            ret new

        get "/checklists" $ authorize $ do
            checklists <- liftIO $ getChecklists conn
            ret checklists
        put "/checklists" $ authorize $ do
            checklist <- jsonData :: ActionM Checklist
            liftIO $ putChecklist conn checklist
            ret checklist

maybeEither e = case e of
  Left _ -> Nothing
  Right m -> Just m

main :: IO ()
main = do
  let resourcePolicy = simpleCorsResourcePolicy {
      corsMethods = ["GET", "POST", "HEAD", "PUT", "DELETE"],
      corsRequestHeaders = ["content-type", "origin"]
  }

  server <- runMaybeT $ do
      credentials <- getConfig "credentials.safe"
      liftIO $ putStrLn "Read config"
      config <- lift $ runReaderT initConnection credentials
      liftIO $ putStrLn "Connected to DB"
      let routed = runReader routes config 
      let server = runReader (startServer resourcePolicy routed) config
      return $ runReader (startServer resourcePolicy routed) config

  guard(isJust server)
  putStrLn "Server up and running"
  fromJust server
