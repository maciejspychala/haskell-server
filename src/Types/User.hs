{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User where

import Types.Imports

data User = User { userId :: Maybe Int,
    firstName :: String,
    lastName :: String,
    teamIds :: [Int] } deriving (Show, Generic) 

instance HasId User where
    getId = userId
    setId a id = a { userId = id }

instance HasArray User where
    setArray conn u = 
        case userId u of
            Nothing -> return u { teamIds = [] }
            Just uid -> do
                teams <- getTeamsByUserId conn uid
                return u { teamIds = teams }

getTeamsByUserId :: Connection -> Int -> IO [Int]
getTeamsByUserId conn uid = do
    ids <- query conn userTeamQuery (Only uid) :: IO [Only Int]
    return $ map fromOnly ids

userTeamQuery = "select teamId from user_team where userId = (?)" :: Query

instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:? "userId" <*>
        v .: "firstName" <*>
        v .: "lastName" <*> 
        pure []

instance ToJSON User

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> pure []

instance ToRow User where
    toRow u = [toField (firstName u), toField (lastName u)]


