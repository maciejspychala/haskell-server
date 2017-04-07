import Auth
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad

main :: IO ()
main = do
    jwk <- readJWK "key.json"
    Right token <- signUser jwk "mewa"
    Right ok <- verifyUser jwk token "mewa" 
    return ok