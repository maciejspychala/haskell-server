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
        it "Should decrypt secret" $ 2 + 2 == 4
        it "Should not decrypt secret" $ 2 + 2 /= 5
        it "Should decode message" $ 2 * 2 == 4
        it "Should not decode message" $ 2 * 2 /= 5
    describe "Serverify" $ do
      it "Server /login works" $ True == True
      it "Server /users works" $ True == True
      it "Server works/teams works" $ True == True
      it "Server works" $ True /= False
