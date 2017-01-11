{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SqlTypes  where

import GHC.Generics
import Data.Aeson (parseJSON, FromJSON, ToJSON, encode, decode, (.:), (.:?), Value(..))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField


data User = User { userId :: Maybe Int,
    firstName :: String,
    lastName :: String,
    team :: Int } deriving (Show, Generic) 


instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:? "userId" <*>
        v .: "firstName" <*>
        v .: "lastName" <*>
        v .: "team"

instance ToJSON User

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field 

instance ToRow User where
    toRow u = [toField (firstName u), toField (lastName u), toField (team u)]

data Team = Team { teamId:: Int,
    name :: String } deriving (Show, Generic) 

instance FromJSON Team where
    parseJSON (Object v) = Team <$>
        v .: "teamId" <*>
        v .: "name"

instance ToJSON Team

instance FromRow Team where
    fromRow = Team <$> field <*> field 

instance ToRow Team where
    toRow u = [toField (teamId u), toField (name u)]
