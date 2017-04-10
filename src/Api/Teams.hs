{-|
Team-related API calls
-}
module Api.Teams
where

import DB
import Data.Int
import Data.Monoid ((<>))
import Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Types

{-|
Gets teams using given PGSQL connection
-}
getTeams :: Connection -> IO [Team]
getTeams conn = selectAll conn allTeamsQuery

{-|
Puts team into DB using given PGSQL connection and returns the new object
-}
putTeam :: Connection -> Team -> IO Team
putTeam conn team = insertInto conn insertTeamQuery team

{-|
Puts team into DB using given PGSQL connection and returns the new object
-}
getTeamById :: Connection -> TL.Text -> IO (Maybe Team)
getTeamById conn id = selectById conn id getTeamQueryById

{-|
Gets task list from a team with given id using given PGSQL connection
-}
getTeamTasks :: Connection -> TL.Text -> IO [Task]
getTeamTasks conn id = selectAllBy conn id (allTasksQuery <> whereTeam)

{-|
Gets team member list from a team with given id using given PGSQL connection
-}
getTeamMembers :: Connection -> TL.Text -> IO [User]
getTeamMembers conn id = getAllWithArrayById conn id teamUsersQuery

{-|
Puts a user with given uid into a team with given tid using given PGSQL connection
-}
putTeamMember :: Connection -> TL.Text -> TL.Text -> IO (TL.Text, TL.Text)
putTeamMember conn tid uid = insertIdId conn insertTeamUserQuery (tid, uid)

{-|
Deletes a user with given uid from a team with given tid using given PGSQL connection
-}
deleteTeamMember :: Connection -> TL.Text -> TL.Text -> IO Int64
deleteTeamMember conn tid uid = deleteIdId conn deleteTeamUserQuery (tid, uid)
