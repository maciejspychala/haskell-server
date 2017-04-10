module Api.Auth (
    logUserIn,
    authorize
) where

import DB
import Web.Scotty
import Auth
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple
import Types
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Either
import Control.Monad
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy.Char8 as L


{-|
Attempts to log in using given credentials
-}
logUserIn :: Connection -> Credentials -> MaybeT IO Token
logUserIn conn (Credentials user passwd) = do
  ok <- lift $ fmap fromOnly $
    checkValVal conn (TL.pack user) (TL.pack passwd) checkCredentialsQuery
  guard (ok == 1)
  jwk <- lift $ readJWK "key.json"
  signed <- lift $ signUser jwk user
  guard (isRight signed)
  let Right token = signed
  return $ Token $ L.unpack token

authorizeUser :: Maybe TL.Text -> MaybeT IO String
authorizeUser auth' = do
  auth <- MaybeT . return $ fmap TL.unpack auth'
  let token = L.pack . unwords . tail . words $ auth
  jwk <- lift $ readJWK "key.json"
  verified <- lift $ verifyUser jwk token ""
  guard (isRight verified)
  return "stub"

{-|
Attempts to authorize user and sets response status accordingly
-}
authorize :: ActionM () -> ActionM ()
authorize success = do
  auth <- header (TL.pack "Authorization") :: ActionM (Maybe TL.Text)
  r <- liftIO $ runMaybeT $ do
      authorizeUser auth
  case r of
    Nothing -> status status401
    Just c -> success
