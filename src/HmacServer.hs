{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HmacServer() where

import Control.Concurrent(threadDelay)
import qualified Data.ByteString as B
import Data.Text.Encoding(encodeUtf8)
import Data.Text(Text)
import Network.HTTP.Types.Status(status400, status500)
import Yesod

import qualified Lib

data HMacForFile = HMacForFile

mkYesod "HMacForFile" [parseRoutes|
/ HomeR GET
|]

instance Yesod HMacForFile

badRequest :: Handler Html
badRequest = do 
    html <- defaultLayout [whamlet|Bad request|] 
    sendResponseStatus status400 html

validationFailure :: Handler Html
validationFailure = do 
    html <- defaultLayout [whamlet| Internal server error|] 
    sendResponseStatus status500 html

hmacKey :: B.ByteString
hmacKey = "key"

delayTime :: Int
delayTime = 50 * 1000

insecureCompare :: B.ByteString -> B.ByteString -> IO Bool
insecureCompare as bs
  | B.length as == 0 && B.length bs == 0 = return True
  | B.length as == 0 = return False
  | B.length bs == 0 = return False
  | B.head as /= B.head bs = return False
  | otherwise = do
      threadDelay delayTime
      return $ insecureCompare (B.tail as) (B.tail bs)

validateSignature :: B.ByteString -> B.ByteString -> IO Bool
validateSignature file sig = compareResult
    where 
        compareResult = insecureCompare sigFromFile sig
        sigFromFile = Lib.bytesToHex $ Lib.hmacSha1 hmacKey file

validationResponse :: Text -> Text -> Handler (IO Html)
validationResponse file sig = 
    let file' = encodeUtf8 file
        sig' = encodeUtf8 sig in
    do 
        validationResult <- validateSignature file' sig' 
        case validationResult of
            True -> defaultLayout [whamlet|congrats!|]
            False -> validationFailure

getHomeR :: Handler Html
getHomeR = do
    fileMaybe <- lookupGetParam "file"
    case fileMaybe of
        Nothing -> badRequest
        Just file -> do
           signatureMaybe <- lookupGetParam "signature"
           case signatureMaybe of
               Nothing -> badRequest
               Just signature -> validationResponse file signature

main :: IO ()
main = warp 3000 HMacForFile
