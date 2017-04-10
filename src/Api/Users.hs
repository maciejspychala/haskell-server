{-|
User-related API calls
-}
module Api.Users (
    getUsers,
    putUser,
    getUserById
) where

import DB
import Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Types

{-|
Gets user list using given PGSQL connection
-}
getUsers :: Connection -> IO [User]
getUsers conn = getWithArray conn allUsersQuery

{-|
Puts user into DB and returns the object created (e.g. with ids etc.)
-}
putUser :: Connection -> User -> IO User
putUser conn user = do
  new <- insertInto conn insertUserQuery user
  setSalary conn new
  return new

{-|
Gets user by id using given PGSQL connection
-}
getUserById :: Connection -> TL.Text -> IO (Maybe User)
getUserById conn id = getWithArrayById conn id getUserQueryById
