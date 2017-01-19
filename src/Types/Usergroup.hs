{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Usergroup where

import Types.Imports

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
