{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth
  -- (
  --   readJWK,
  --   signUser,
  --   verifyUser
    -- )
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

right :: Either a b -> b
right (Right a) = a
right (Left a) = error "right: Unexpected Left"

readJWK :: String -> IO JWK
readJWK fname = do
    str <- L.readFile fname
    return $ fromMaybe (error "Error while creating JWK") . decode $ str

signUser :: JWK -> String -> IO (Either Error ByteString)
signUser jwk user = do 
    let alg = JWSAlg RS256
        header = newJWSHeader (Protected, RS256)
        issuer = fromString . T.pack $ "mewa@github.com"
        audience = Audience [fromString . T.pack $ user]
        claims = (set claimAud (Just audience)) 
            . (set claimIss (Just issuer)) 
            $ emptyClaimsSet
    runExceptT $ createJWSJWT jwk header claims >>= encodeCompact

verifyUser :: JWK -> ByteString -> String -> IO (Either JWTError ())
verifyUser jwk token aud = do 
    let aud' = fromString $ T.pack aud
        validation = (set jwtValidationSettingsAudiencePredicate (==aud')) $ defaultJWTValidationSettings
    runExceptT $ decodeCompact token >>= validateJWSJWT validation jwk

data Credentials = Credentials {
    login :: String,
    password :: String
} deriving (Show, Generic)

instance FromJSON Credentials where
    parseJSON (Object v) = Credentials <$>
        v .: "login" <*>
        v .: "password"

data Token = Token {
  token :: String
} deriving (Show, Generic)

instance ToJSON Token
