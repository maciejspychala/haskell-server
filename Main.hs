{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Text.Lazy 
import Data.Monoid ((<>))
import SqlTypes
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics
import Database.SQLite.Simple
import qualified Data.ByteString.Lazy.Char8 as BS

routes :: [User] -> ScottyM ()
routes allUsers = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name
    get "/users" $ do
        json allUsers
    get "/users/:id" $ do
        id <- param "id"
        json ([x | x <- allUsers, userId x == id] !! 0)

hello :: ActionM ()
hello = do
    text "hello world!"

helloName :: Text -> ActionM ()
helloName name = do
    text ("hello " <> name <> " :*")

main = do
    conn <- open "data.db"
    users <- ( query_ conn "select id, first_name, second_name, team from users" :: IO [User] )
    scotty 3000 (routes users)
