{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This module contains functions used for creation and validation of JWT tokens
-}
module Auth
where

import Crypto.JWT
import Crypto.JOSE.JWK
import Crypto.JOSE.JWS
import Crypto.JOSE.Header
import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Except
import Crypto.JOSE.Error
import Crypto.JOSE.Compact
import Data.ByteString.Lazy.Char8 as L
import Data.Text as T
import Control.Monad
import Data.Maybe
import Data.Either
import Control.Lens
import Control.Monad.State.Lazy
import GHC.Generics
import Types.Imports
import Data.Time.Clock
import Control.Monad.Reader

right :: Either a b -> b
right (Right a) = a
right (Left a) = error "right: Unexpected Left"


{-|
Reads JWK from file at given path
-}
readJWK :: String -> IO JWK
readJWK fname = do
    str <- L.readFile fname
    return $ fromMaybe (error "Error while creating JWK") . decode $ str

{-|
Signs standard app JWS claims and returns encoded JWT token
-}
signUser :: JWK -> String -> IO (Either Error ByteString)
signUser jwk user = do 
    time <- getCurrentTime
    let alg = JWSAlg RS256
        header = newJWSHeader (Protected, RS256)
        issuer = fromString . T.pack $ "mewa@github.com"
        audience = Audience [fromString . T.pack $ user]
        claims = (set claimAud (Just audience)) 
            . (set claimIss (Just issuer)) 
            . (set claimIat (Just $ NumericDate time))
            $ emptyClaimsSet
    runExceptT $ createJWSJWT jwk header claims >>= encodeCompact

{-|
Verifies JWT token using given JWK
-}
verifyUser :: JWK -> ByteString -> String -> IO (Either JWTError ())
verifyUser jwk token aud = do 
    let aud' = fromString $ T.pack aud
        validation = (set jwtValidationSettingsAudiencePredicate (/= "")) $ defaultJWTValidationSettings
    runExceptT $ decodeCompact token >>= validateJWSJWT validation jwk

{-|
Record representing credentials used to log in
-}
data Credentials = Credentials {
    login :: String,
    password :: String
} deriving (Show, Generic)

instance FromJSON Credentials where
    parseJSON (Object v) = Credentials <$>
        v .: "login" <*>
        v .: "password"

{-|
Record representing return response JWT tokens
-}
data Token = Token {
  token :: String
} deriving (Show, Generic)

instance ToJSON Token

data Authorization = Authorization {
  name :: String,
  permissions :: [String]
} deriving (Show, Generic)
