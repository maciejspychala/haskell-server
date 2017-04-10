{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Typeclasses where

import Database.PostgreSQL.Simple

{-|
Typeclass for data having an id
-}
class HasId a where
    getId :: a -> Maybe Int
    setId :: a -> Maybe Int -> a

{-|
Typeclass for data having an array
-}
class (HasId a) => HasArray a where
    setArray :: Connection -> a -> IO a
