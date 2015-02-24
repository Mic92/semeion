module Handler.Home where

import Import
import Network.Wai
import Text.Regex.TDFA
import System.Process
import System.Exit
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding  as TE
import qualified Data.List as L
import qualified Data.List.Split as LS

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  rawHostnames <- lookupGetParam "hostname"
  hostnames <- case rawHostnames of
    Nothing -> return Nothing
    Just a -> return $ Just $ T.splitOn "," a
  rawMyip <- lookupGetParam "myip"
  requestIp <- case rawMyip of
    Nothing -> lookupHeader "x-forwarded-for"
    Just a  -> return $ Just ("bla" :: ByteString)
  myip <- case rawMyip of
    Nothing -> do
      rawIp <- case requestIp of
        Nothing ->
          (show . remoteHost . reqWaiRequest) <$> getRequest
        Just a ->
          return $ T.unpack $ TE.decodeUtf8 a
      return $ extractIp rawIp
    Just ip ->
      return $ T.unpack ip
  wildcard <- lookupGetParam "wildcard"
  mx <- lookupGetParam "mx"
  backmx <- lookupGetParam "backmx"
  offline <- lookupGetParam "offline"
  case hostnames of
    Nothing -> do
      sendResponse ("notfqdn" :: T.Text)
    Just names -> do
      case L.length names > 20 of
        True -> do
          sendResponse ("numhosts" :: T.Text)
        False -> do
          res <- return $ map checkFQDN names
          ans <- return $ map (nsupdate myip) res
          ans2 <- sequence ans
          out <- return $ map (\a -> case a of
            Right y -> y >>= return . errNoErr
            Left x -> return $ x
            ) ans2
          out2 <- liftIO $ sequence out
          sendResponse $ T.intercalate "\n" out2

errNoErr x =
  case x of
    True -> ("good" :: T.Text)
    False -> ("dnserr" :: T.Text)

-- checkHostnames :: Maybe [T.Text] -> Either T.Text [T.Text]
checkhostnames names =
  case names of
    Nothing -> Left "notfqdn"
    Just list -> case L.length list > 20 of
      True -> Left "numhosts"
      False -> Right list

checkDnsErr a =
  case a of
    (ExitSuccess, _, _) -> True
    (ExitFailure _, _, _) -> False

checkFQDN :: T.Text -> Either T.Text T.Text
checkFQDN name =
  case T.unpack name =~ ("^([[:digit:]a-zA-Z]([-[:digit:]a-zA-Z]{0,61}[[:digit:]a-zA-Z]){0,1})$" :: String) :: Bool of
    False -> Right name
    True -> Left ("notfqdn" :: T.Text)

nsupdate ip res =
  case res of
    Left a -> return $ Left a
    Right name -> 
      return $ Right answer
      where
        answer = readProcessWithExitCode "/usr/bin/nsupdate" [] ("update add " ++ T.unpack name ++ " 8640 A " ++ ip ++ " \n send") >>= return . checkDnsErr

extractIpv6 :: String -> String
extractIpv6 rawIp =
  case '.' `elem` rawIp of
    False -> L.take (1 + (fromMaybe 39 $ L.findIndex ( == ']') rawIp)) rawIp
    True -> drop 8 (fst (L.splitAt (fromMaybe 12 $ L.findIndex ( == ']') rawIp) rawIp))

extractIpv4 :: String -> String
extractIpv4 rawIp =
  fst (L.splitAt (fromMaybe 12 $ L.findIndex ( == ':') rawIp) rawIp)

extractIp rawIp =
  case '[' `elem` rawIp of
    True -> extractIpv6 rawIp
    False -> extractIpv4 rawIp
