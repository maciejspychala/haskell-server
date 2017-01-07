{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SqlTypes  where

import GHC.Generics
import Data.Aeson (parseJSON, FromJSON, ToJSON, encode, decode, (.:), (.:?), Value(..))
import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField



data User = User { userId :: Maybe Int,
    firstName :: String,
    lastName :: String,
    teamId :: Int } deriving (Show, Generic) 


instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:? "userId" <*>
        v .: "firstName" <*>
        v .: "lastName" <*>
        v .: "teamId"

instance ToJSON User

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field 

instance ToRow User where
    toRow u = [toField (firstName u), toField (lastName u), toField (teamId u)]


