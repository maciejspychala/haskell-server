{-# LANGUAGE DeriveGeneric #-}
module SqlTypes  where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, encode)

data User = User { userId :: Int,
    firstName :: String,
    lastName :: String } deriving (Show, Generic) 

instance ToJSON User
