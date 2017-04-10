{-|
Module containing API call functions
-}
module Api (
    module Api.Users,
    module Api.Teams,
    module Api.Tasks,
    module Api.Events,
    module Api.Auth,
    getChecklists,
    putChecklist
) where

import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Auth
import DB
import Database.PostgreSQL.Simple
import Types
import Config
import Data.Int
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as L
import Web.Scotty
import Control.Monad.IO.Class
import Api.Users
import Api.Teams
import Api.Tasks
import Api.Events
import Api.Auth


getChecklists :: Connection -> IO [Checklist]
getChecklists conn = getWithArray conn allChecklistsQuery

putChecklist :: Connection -> Checklist -> IO ()
putChecklist conn checklist = insertChecklist conn checklist
