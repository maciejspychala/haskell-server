{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User where

import Types.Imports

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


