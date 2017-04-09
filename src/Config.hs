{-# LANGUAGE OverloadedStrings #-}

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


getCredentials :: String -> Maybe (String, String, String, String, String, Int)
getCredentials str = return (lines str) >>= \conf -> case conf of
  [a, b, c, d, e, f] -> return (a, b, c, d, e, read f)
  _ -> Nothing

connStringFromCredentials :: (String, String, String, String, String, Int) -> Maybe BS.ByteString
connStringFromCredentials (server, serverPort, dbname, username, password, portString) =
  return $ ("host='" <> (BS.pack server) <> "' port='" <> (BS.pack serverPort)
             <> "' user='" <> (BS.pack username) <> "' dbname='" <> (BS.pack dbname)
             <> "' password='" <> (BS.pack password) <> "'")

createConnection :: BS.ByteString -> Maybe (IO Connection)
createConnection connStr = return $ connectPostgreSQL connStr

startServer :: IO Connection -> CorsResourcePolicy -> Int -> (Connection -> ScottyM ()) -> IO ()
startServer ioconn resourcePolicy port routes = ioconn >>= \conn -> scotty port $ do
  middleware $ cors (const $ Just resourcePolicy)
  routes $ conn
