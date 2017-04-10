-- | Events-related API calls
module Api.Events
where

import DB
import Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Types

{-|
Gets events using given PGSQL connection
-}
getEvents :: Connection -> IO [Event]
getEvents conn = selectAll conn allEventsQuery

{-|
Gets event by id using given PGSQL connection
-}
getEventById :: Connection -> TL.Text -> IO (Maybe Event)
getEventById conn id = selectById conn id getEventQueryById

{-|
Puts event into DB using given PGSQL connection
-}
putEvent :: Connection -> Event -> IO Event
putEvent conn event = insertInto conn insertEventQuery event
