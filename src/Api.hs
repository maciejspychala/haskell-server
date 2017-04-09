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
    getEventbyId,
    putEvent,
    getChecklists,
    putChecklist
) where

import Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Auth
import DB
import Database.PostgreSQL.Simple
import Types
import Config
import Data.Int

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

getEventById :: Connection -> TL.Text -> IO [Event]
getEventById conn id = selectById conn id getEventQueryById

putEvents :: Connection -> Event -> IO [Event]
putEvents conn event = insertInto conn insertEventQuery event

getChecklists :: Connection -> IO [Checklist]
getChecklists conn = getWithArray conn allChecklistsQuery

putChecklist :: Connection -> IO Checklist
putChecklist conn = insertChecklist conn checklist
