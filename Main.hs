{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Text.Lazy 
import Data.Monoid ((<>))
import Control.Monad.IO.Class
import SqlTypes
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics
import Database.SQLite.Simple
import qualified Data.ByteString.Lazy.Char8 as BS

routes :: Connection -> ScottyM ()
routes conn = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name
    get "/users" $ do
        users <- liftIO ( query_ conn "select id, first_name, second_name, team from users" :: IO [User] )
        json users
    get "/users/:id" $ do
        id <- param "id"
        users <- liftIO ( query_ conn "select id, first_name, second_name, team from users" :: IO [User] )
        json ([x | x <- users, userId x == id] !! 0)


hello :: ActionM ()
hello = do
    text "hello world!"

helloName :: Text -> ActionM ()
helloName name = do
    text ("hello " <> name <> " :*")

main = do
    conn <- open "data.db"
    scotty 3000 (routes conn)
