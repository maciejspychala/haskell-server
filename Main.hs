{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Text.Lazy 
import Data.Monoid ((<>))
import SqlTypes
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS

maciek :: User
maciek = User { userId = 1, firstName = "maciek", lastName = "freeman" }

morgan :: User
morgan = User { userId = 2, firstName = "morgan", lastName = "freeman" }

alex :: User
alex = User { userId = 3, firstName = "alex", lastName = "freeman" }

allUsers :: [User]
allUsers = [maciek, morgan, alex]


routes :: ScottyM ()
routes = do
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
    putStrLn(firstName maciek)
    BS.putStrLn(encode maciek)
    scotty 3000 routes
