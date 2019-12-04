{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HmacServer() where
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

validateSignature :: B.ByteString -> B.ByteString -> Bool
validateSignature file sig = sigFromFile == sig
    where sigFromFile = Lib.bytesToHex $ Lib.hmacSha1 hmacKey file

validationResponse :: Text -> Text -> Handler Html
validationResponse file sig = 
    let file' = encodeUtf8 file
        sig' = encodeUtf8 sig in
    case validateSignature file' sig' of
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
