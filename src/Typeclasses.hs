{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Typeclasses where

import Database.PostgreSQL.Simple

class HasId a where
    getId :: a -> Maybe Int
    setId :: a -> Maybe Int -> a

class (HasId a) => HasArray a where
    setArray :: Connection -> a -> IO a
