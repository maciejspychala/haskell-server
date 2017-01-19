{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Task where

import Types.Imports
import Data.Time.Clock

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
