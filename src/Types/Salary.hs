{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Salary where

import Types.Imports

data Salary = Salary { salaryId :: Int,
    money :: Double } deriving (Show, Generic)

instance ToJSON Salary

instance FromJSON Salary where
    parseJSON (Object v) = Salary <$>
        v .: "salaryId" <*>
        v .: "money"
