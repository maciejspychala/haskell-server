module Api (
    getUsers,
    putUser,
    getUserById,
    getTeams,
    putTeam,
    getTeamById,
    getTeamTasks,
    getTeamMembers,
    putTeamMember,
    deleteTeamMember,
    getTasks,
    getTaskById,
    putTask,
    getTaskChecklist,
    getEvents,
    getEventById,
    putEvent,
    getChecklists,
    putChecklist,
    logUserIn,
    authorizeUser,
    authorize
) where

import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Auth
import DB
import Database.PostgreSQL.Simple
import Types
import Config
import Data.Int
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as L
import Web.Scotty
import Control.Monad.IO.Class
import Network.HTTP.Types.Status

getUsers :: Connection -> IO [User]
getUsers conn = getWithArray conn allUsersQuery

putUser :: Connection -> User -> IO User
putUser conn user = do
  new <- insertInto conn insertUserQuery user
  setSalary conn new
  return new

getUserById :: Connection -> TL.Text -> IO (Maybe User)
getUserById conn id = getWithArrayById conn id getUserQueryById

getTeams :: Connection -> IO [Team]
getTeams conn = selectAll conn allTeamsQuery

putTeam :: Connection -> Team -> IO Team
putTeam conn team = insertInto conn insertTeamQuery team

getTeamById :: Connection -> TL.Text -> IO (Maybe Team)
getTeamById conn id = selectById conn id getTeamQueryById


getTeamTasks :: Connection -> TL.Text -> IO [Task]
getTeamTasks conn id = selectAllBy conn id (allTasksQuery <> whereTeam)

getTeamMembers :: Connection -> TL.Text -> IO [User]
getTeamMembers conn id = getAllWithArrayById conn id teamUsersQuery

putTeamMember :: Connection -> TL.Text -> TL.Text -> IO (TL.Text, TL.Text)
putTeamMember conn tid uid = insertIdId conn insertTeamUserQuery (tid, uid)

deleteTeamMember :: Connection -> TL.Text -> TL.Text -> IO Int64
deleteTeamMember conn tid uid = deleteIdId conn deleteTeamUserQuery (tid, uid)

getTasks :: Connection -> IO [Task]
getTasks conn = selectAll conn allTasksQuery

getTaskById :: Connection -> TL.Text -> IO (Maybe Task)
getTaskById conn id = selectById conn id getTaskQueryById

putTask :: Connection -> Task -> IO Task
putTask conn task = insertInto conn insertTaskQuery task

getTaskChecklist :: Connection -> TL.Text -> IO (Maybe Checklist)
getTaskChecklist conn id = getWithArrayById conn id getChecklistByOwner

getEvents :: Connection -> IO [Event]
getEvents conn = selectAll conn allEventsQuery

getEventById :: Connection -> TL.Text -> IO (Maybe Event)
getEventById conn id = selectById conn id getEventQueryById

putEvent :: Connection -> Event -> IO Event
putEvent conn event = insertInto conn insertEventQuery event

getChecklists :: Connection -> IO [Checklist]
getChecklists conn = getWithArray conn allChecklistsQuery

putChecklist :: Connection -> Checklist -> IO ()
putChecklist conn checklist = insertChecklist conn checklist

logUserIn :: Connection -> Credentials -> MaybeT IO Token
logUserIn conn (Credentials user passwd) = do
  ok <- lift $ fmap fromOnly $
    checkValVal conn (TL.pack user) (TL.pack passwd) checkCredentialsQuery
  guard (ok == 1)
  jwk <- lift $ readJWK "key.json"
  signed <- lift $ signUser jwk user
  guard (isRight signed)
  let Right token = signed
  return $ Token $ L.unpack token

authorizeUser :: Maybe TL.Text -> MaybeT IO String
authorizeUser auth' = do
  auth <- MaybeT . return $ fmap TL.unpack auth'
  let token = L.pack . unwords . tail . words $ auth
  jwk <- lift $ readJWK "key.json"
  verified <- lift $ verifyUser jwk token ""
  guard (isRight verified)
  return "stub"

authorize :: ActionM () -> ActionM ()
authorize success = do
  auth <- header (TL.pack "Authorization") :: ActionM (Maybe TL.Text)
  r <- liftIO $ runMaybeT $ do
      authorizeUser auth
  case r of
    Nothing -> status status401
    Just c -> success
