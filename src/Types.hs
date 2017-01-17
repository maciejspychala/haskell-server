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

class HasId a where
    getId :: a -> Maybe Int
    setId :: a -> Maybe Int -> a

data User = User { userId :: Maybe Int,
    firstName :: String,
    lastName :: String,
    team :: Int } deriving (Show, Generic) 

instance HasId User where
    getId a = userId a
    setId a id = a { userId = id }

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


instance HasId Team where
    getId a = teamId a
    setId a id = a { teamId = id }

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

instance HasId Task where
    getId a = taskId a
    setId a id = a { taskId = id }

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


instance HasId Event where
    getId a = eventId a
    setId a id = a { eventId = id }

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

instance HasId Checklist where
    getId a = checklistId a
    setId a id = a { checklistId = id }

instance ToRow Checklist where
    toRow c = [toField $ listOwner c]

instance ToJSON Checklist

instance FromJSON Checklist where
    parseJSON (Object v) = Checklist <$>
        v .:? "checklistId" <*>
        v .: "listOwner" <*>
        v .: "checklistItems"

data ChecklistItem = ChecklistItem { checklistItemId :: Maybe Int,
    itemText :: String,
    finished :: Bool,
    checklist :: Int } deriving (Show, Generic)

instance HasId ChecklistItem where
    getId a = checklistItemId a
    setId a id = a { checklistItemId = id }

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

data Usergroup = Usergroup { usergroupId :: Maybe Int,
    usergroupName :: String,
    permissions :: [Permission] } deriving (Show, Generic)

instance HasId Usergroup where 
    getId a = usergroupId a
    setId a id = a { usergroupId = id }

instance ToRow Usergroup where 
    toRow u = [toField $ usergroupName u]

instance ToJSON Usergroup

instance FromJSON Usergroup where
    parseJSON (Object v) = Usergroup <$>
        v .:? "usergroupId" <*>
        v .: "usergroupName" <*>
        v .: "permissions"

data Permission = Permission { permissionId :: Maybe Int,
    permissionName :: String } deriving (Show, Generic)

instance HasId Permission where
    getId p = permissionId p
    setId p id = p { permissionId = id }

instance FromRow Permission where
    fromRow = Permission <$> field <*> field

instance ToRow Permission where
    toRow p = [toField $ permissionName p]

instance ToJSON Permission

instance FromJSON Permission where
    parseJSON (Object v) = Permission <$>
        v .:? "permissionId" <*>
        v .: "permissionName"
