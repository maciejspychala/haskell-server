{-# LANGUAGE OverloadedStrings #-}
{-|
Module containing helper functions for initialization &  setting the server up and running
-}
module Config(
    getConfig,
    createConnection,
    startServer,
    Config, dbHost, dbPort, dbName, dbUser, dbPassword, serverPort, connection,
    initConnection
) where

import Data.Monoid ((<>))
import Control.Monad.State.Lazy as ST
import Web.Scotty
import Database.PostgreSQL.Simple
import Network.Wai.Middleware.Cors
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

data Config = Config {
  dbHost :: String,
  dbPort :: String,
  dbName :: String,
  dbUser :: String,
  dbPassword :: String,
  serverPort :: Int,
  connection :: Maybe Connection
}

{-|
Reads config from file at give path
-}
getConfig :: String -> MaybeT IO Config
getConfig path = (liftIO $ readFile path) >>= parseConfig

{-|
Parses config string
-}
parseConfig :: String -> MaybeT IO Config
parseConfig str = return (lines str) >>= \conf ->
    case conf of
      [a, b, c, d, e, f] -> return $ Config {dbHost = a, dbPort = b, dbName = c, dbUser = d, dbPassword = e, serverPort = read f, connection = Nothing }
      _ -> MaybeT . return $ Nothing

{-|
Creates DB connection string used for connecting to DB
-}
connStringFromCredentialsT :: (Monad m) => ReaderT Config m BS.ByteString
connStringFromCredentialsT = do
    host <- BS.pack <$> asks dbHost
    port <- BS.pack <$> asks dbPort
    dbName <- BS.pack <$> asks dbName
    user <- BS.pack <$> asks dbUser
    pwd <- BS.pack <$> asks dbPassword
    return $ ("host='" <> host <> "' port='" <> port
             <> "' user='" <> user <> "' dbname='" <> dbName
             <> "' password='" <> pwd <> "'")

initConnection :: ReaderT Config IO Config
initConnection = do
    connStr <- connStringFromCredentialsT
    port <- asks dbPort
    conf <- ask
    connection <- liftIO $ createConnection connStr
    return conf { connection = Just connection }

{-|
Creates DB connection using given connection string
-}
createConnection :: BS.ByteString -> (IO Connection)
createConnection connStr = connectPostgreSQL connStr

{-|
Starts server with given DB connection, resource policy and routing
-}
startServer :: CorsResourcePolicy -> ScottyM () -> Reader Config (IO ())
startServer resourcePolicy routes = do
    port <- asks serverPort
    return $ scotty port $ do
        middleware $ cors (const $ Just resourcePolicy)
        routes
