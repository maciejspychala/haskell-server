import Auth
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString as BS
import Control.Monad
import Test.Hspec
import Data.Either
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Config
import Control.Exception (evaluate)
import Control.Monad.Identity
import Web.Scotty
import Network.Wai.Middleware.Cors
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

main :: IO ()
main = do
    authTests

authTests = hspec$  do 
    describe "Auth" $ do
        it "should read valid JWK" $ do
            readJWK "key.json" >> return ()
        it "should not read invalid JWK" $ do
            shouldThrow (readJWK "invalidkey.json") anyIOException
        it "should successfully verify a generated token" $ do
            jwk <- readJWK "key.json"
            Right token <- signUser jwk "mewa"
            Right ok <- verifyUser jwk token "mewa" 
            return ok
        it "should not verify wrong user" $ do
            jwk <- readJWK "key.json"
            Right token <- signUser jwk "mewa"
            verified <- verifyUser jwk token "mewax" 
            guard(isLeft verified)
        it "should not verify wrong user" $ do
            shouldThrow (readJWK "key.jsonx") anyException
    describe "Config" $ do
        it "should parse valid config" $ do
            s <- runMaybeT $ getConfig "credentials.safe"
            guard(isJust s)
        it "should fail parsing non-existent config" $ do
            shouldThrow (runMaybeT $ getConfig "nonexistent.unsafe") anyIOException
        it "should return Nothing when parsing invalid config" $ do
            conf <- runMaybeT $ parseConfig "xaxaxa"
            guard(isNothing conf) :: IO ()
    describe "Connection" $ do
        it "should construct connstring" $ do
          Just conf <- runMaybeT $ getConfig "credentials.safe"
          connstr <- runReaderT connStringFromCredentialsT conf
          guard((>0) . BS.length $ connstr)
    describe "Server" $ do
      it "should start server successfully" $ do
          let routed = notFound (raw $ L.pack "x")
              resourcePolicy = simpleCorsResourcePolicy
          server <- runMaybeT $ do
            credentials <- getConfig "credentials.safe"
            config <- lift $ runReaderT initConnection credentials
            let server = runReader (startServer resourcePolicy routed) config
            return $ runReader (startServer resourcePolicy routed) config
          guard(isJust server)         
