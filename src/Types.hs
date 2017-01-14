{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics
import Data.Aeson (parseJSON, FromJSON, ToJSON, encode, decode, (.:), (.:?), Value(..))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Data.Time.Clock


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

data Team = Team { teamId :: Maybe Int,
    name :: String } deriving (Show, Generic) 

instance FromJSON Team where
    parseJSON (Object v) = Team <$>
        v .:? "teamId" <*>
        v .: "name"

instance ToJSON Team

instance FromRow Team where
    fromRow = Team <$> field <*> field 

instance ToRow Team where
    toRow u = [toField (name u)]


data Task = Task { taskId :: Maybe Int,
    beginDate :: UTCTime,
    endDate :: UTCTime,
    exeqTeam :: Int,
    description :: String } deriving (Show, Generic)


instance FromRow Task where
    fromRow = Task <$> field <*> field <*> field <*> field <*> field 

instance ToRow Task where
     toRow t = [toField (beginDate t), toField (endDate t),
        toField (exeqTeam t), toField (description t)]

instance ToJSON Task

instance FromJSON Task where
    parseJSON (Object v) = Task <$>
        v .:? "taskId" <*>
        v .: "beginDate" <*>
        v .: "endDate" <*>
        v .: "exeqTeam" <*>
        v .: "description"

data Event = Event { eventId :: Maybe Int,
    eventName :: String,
    creator :: Int } deriving (Show, Generic)

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field

instance ToRow Event where 
    toRow e = [toField $ eventName e, toField $ creator e]

instance ToJSON Event

instance FromJSON Event where
    parseJSON (Object v) = Event <$>
        v .:? "eventId" <*>
        v .: "eventName" <*>
        v .: "creator"

data Checklist = Checklist { checklistId :: Maybe Int,
    listOwner :: Int,
    checklistItems :: [ChecklistItem]} deriving (Show, Generic)

instance ToRow Checklist where
    toRow c = [toField $ listOwner c]

instance ToJSON Checklist

instance FromJSON Checklist where
    parseJSON (Object v) = Checklist <$>
        v .:? "checklistId" <*>
        v .: "teamListOwner" <*>
        v .: "checklistItems"

data ChecklistItem = ChecklistItem { checklistItemId :: Maybe Int,
    itemText :: String,
    finished :: Bool,
    checklist :: Int } deriving (Show, Generic)

instance FromRow ChecklistItem where
    fromRow = ChecklistItem <$> field <*> field <*> field <*> field

instance ToRow ChecklistItem where
    toRow i = [toField $ itemText i, toField $ finished i, toField $ checklist i]

instance ToJSON ChecklistItem

instance FromJSON ChecklistItem where
    parseJSON (Object v) = ChecklistItem <$>
        v .:? "checklistItemId" <*>
        v .: "itemText" <*>
        v .: "finished" <*>
        v .: "checklist"
