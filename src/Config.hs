{-# LANGUAGE OverloadedStrings #-}
{-|
Module containing helper functions for initialization &  setting the server up and running
-}
module Config(
    getCredentials,
    connStringFromCredentials,
    createConnection,
    startServer
) where

import Data.Monoid ((<>))
import Control.Monad.State.Lazy as ST
import Web.Scotty
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Cors
import qualified Data.ByteString.Char8 as BS

{-|
Reads config from file at give path
-}
getCredentials :: String -> Maybe (String, String, String, String, String, Int)
getCredentials str = return (lines str) >>= \conf -> case conf of
  [a, b, c, d, e, f] -> return (a, b, c, d, e, read f)
  _ -> Nothing

{-|
Creates DB connection string used for connecting to DB
-}
connStringFromCredentials :: (String, String, String, String, String, Int) -> Maybe BS.ByteString
connStringFromCredentials (server, serverPort, dbname, username, password, portString) =
  return $ ("host='" <> (BS.pack server) <> "' port='" <> (BS.pack serverPort)
             <> "' user='" <> (BS.pack username) <> "' dbname='" <> (BS.pack dbname)
             <> "' password='" <> (BS.pack password) <> "'")

{-|
Creates DB connection using given connection string
-}
createConnection :: BS.ByteString -> Maybe (IO Connection)
createConnection connStr = return $ connectPostgreSQL connStr

{-|
Starts server with given DB connection, resource policy and routing
-}
startServer :: IO Connection -> CorsResourcePolicy -> Int -> (Connection -> ScottyM ()) -> IO ()
startServer ioconn resourcePolicy port routes = ioconn >>= \conn -> scotty port $ do
  middleware $ cors (const $ Just resourcePolicy)
  routes $ conn
