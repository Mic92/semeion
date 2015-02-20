module Handler.Home where

import Import
import Network.Wai
import Data.Maybe
import qualified Data.Text as T
import qualified Data.List as L

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  rawIp <- (show . remoteHost . reqWaiRequest) <$> getRequest
  ip <- case '[' `L.elem` rawIp of
    True -> return $ extractIpv6 rawIp
    False -> return $ extractIpv4 rawIp
  user <- lookupGetParam "user"
  passwd <- lookupGetParam "passwd"
  defaultLayout $ do
    $( widgetFile "home")

extractIpv6 :: String -> String
extractIpv6 rawIp =
  case '.' `elem` rawIp of
    False -> L.take (1 + (fromJust $ L.findIndex ( == ']') rawIp)) rawIp
    True -> drop 8 (fst (L.splitAt (fromJust $ L.findIndex ( == ']') rawIp) rawIp))

extractIpv4 :: String -> String
extractIpv4 rawIp =
  fst (L.splitAt (fromJust $ L.findIndex ( == ':') rawIp) rawIp)
