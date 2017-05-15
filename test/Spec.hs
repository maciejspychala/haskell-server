import Auth
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import Test.Hspec

main :: IO ()
main = do
    authTests

authTests = hspec $ do 
    describe "Auth" $ do
        it "should successfully verify a generated token" $ do
            jwk <- readJWK "key.json"
            Right token <- signUser jwk "mewa"
            Right ok <- verifyUser jwk token "mewa" 
            return ok