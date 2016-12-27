{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Text.Lazy as T
import Data.Monoid ((<>))

routes :: ScottyM ()
routes = do
    get "/hello" 
        hello
    get "/hello/:name" $ do
        name <- param "name"
        helloName name


hello :: ActionM ()
hello = do
    text "hello world!"

helloName :: Text -> ActionM ()
helloName name = do
    text ("hello " <> name <> " :*")

main = scotty 3000 routes
