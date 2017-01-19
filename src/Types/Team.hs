{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Team where

import Types.Imports

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

