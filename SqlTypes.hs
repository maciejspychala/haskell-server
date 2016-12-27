{-# LANGUAGE DeriveGeneric #-}
module SqlTypes  where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, encode)
import Database.SQLite.Simple

data User = User { userId :: Int,
    lastName :: String,
    firstName :: String,
    teamId :: Int } deriving (Show, Generic) 


instance ToJSON User

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field 
