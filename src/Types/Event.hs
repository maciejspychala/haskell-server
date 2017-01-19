{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Event where

import Types.Imports

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


