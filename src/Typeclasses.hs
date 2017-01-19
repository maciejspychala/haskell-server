{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Typeclasses where

import Database.PostgreSQL.Simple

class HasId a where
    getId :: a -> Maybe Int
    setId :: a -> Maybe Int -> a

class HasArray a where
    setArray :: Connection -> a -> IO a


