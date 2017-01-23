{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Checklist where

import Types.Imports

data Checklist = Checklist { checklistId :: Maybe Int,
    listOwner :: Int,
    checklistItems :: [ChecklistItem]} deriving (Show, Generic)

instance FromRow Checklist where
    fromRow = Checklist <$> field <*> field <*> pure []

instance HasId Checklist where
    getId a = checklistId a
    setId a id = a { checklistId = id }

instance HasArray Checklist where
    setArray conn a = do 
        xs <- getChecklistsItems conn $ listOwner a
        return a { checklistItems = xs }


getChecklistsItems :: Connection -> Int -> IO [ChecklistItem]
getChecklistsItems conn checkId = 
    query conn getChecklistItemsQueryByTeamId (Only checkId)

getChecklistItemsQueryByTeamId = "select id, name, finished, checklist from checklistitems where checklist = (?)" :: Query

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
