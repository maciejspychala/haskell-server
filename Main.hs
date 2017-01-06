{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Control.Monad.IO.Class
import SqlTypes
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import qualified Data.ByteString.Lazy.Char8 as BS

getUserQuery = "select id, first_name, second_name, team from users"
getUserQueryId = "select id, first_name, second_name, team from users where id = (?)"

maciek :: User
maciek = User {userId = 1, firstName = "maciek", lastName = "freeman", teamId = 3 }

instance ToRow User where
    toRow a = [toField (userId a)]


routes :: Connection -> ScottyM ()
routes conn = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name
    get "/users" $ do
        users <- liftIO ( query_ conn getUserQuery :: IO [User] )
        json users
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        users <- liftIO ( (query conn "select id, first_name, second_name, team from users where id = (?)" (Only id) ):: IO [User] )
        json ( head users )


hello :: ActionM ()
hello = do
    text "hello world!"

helloName :: TL.Text -> ActionM ()
helloName name = do
    text ("hello " <> name <> " :*")

main = do
    conn <- open "data.db"
    scotty 3000 (routes conn)
