{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HmacServer(runServer, testHmacServer) where

import Control.Concurrent(forkIO, threadDelay)
import qualified Data.ByteString as B
import Data.IORef
import Data.Text.Encoding(encodeUtf8)
import Data.Text(Text)
import Debug.Trace
import Network.HTTP.Simple
import Network.HTTP.Types.Status(status400, status500)
import Yesod

import qualified Lib

data HMacForFile = HMacForFile { delayTime :: Int }

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

insecureCompare :: Int ->  B.ByteString -> B.ByteString -> IO Bool
insecureCompare delayTime as bs
  | B.length as == 0 && B.length bs == 0 = return True
  | B.length as == 0 = return False
  | B.length bs == 0 = return False
  | B.head as /= B.head bs = return False
  | otherwise = do
      threadDelay delayTime
      insecureCompare delayTime (B.tail as) (B.tail bs)

validateSignature :: Int -> B.ByteString -> B.ByteString -> IO Bool
validateSignature delayTime file sig = compareResult
    where 
        compareResult = insecureCompare delayTime sigFromFile sig
        sigFromFile = Lib.bytesToHex $ Lib.hmacSha1 hmacKey file

validationResponse :: Text -> Text -> Handler (Html)
validationResponse file sig = 
    let file' = encodeUtf8 file
        sig' = encodeUtf8 sig in
    do 
        delayTime <- fmap delayTime getYesod
        validationResult <- liftIO $ validateSignature delayTime file' sig' 
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

runServer :: Int -> IO ()
runServer delayTime = do
    warp 3000 HMacForFile
        { delayTime = delayTime }
                    
-- Testing
url :: Request
url = "http://localhost:3000"

url' :: Request
url' = "http://localhost:3000?file=fancyfilename&signature=f3477b6f15aef01a2f5b90df0da6c0ac619dfe71"

url'' :: Request
url''= "http://localhost:3000?file=fancyfilename&signature=03477b6f15aef01a2f5b90df0da6c0ac619dfe71"

testHmacServer :: IO Bool
testHmacServer = do
    forkIO $ runServer (50*1000)
    response <- httpBS url
    response' <- httpBS url'
    response'' <- httpBS url''

    return $ [400, 200, 500] == map getResponseStatusCode [response, response', response'']

main :: IO ()
main = do
    result <- testHmacServer
    print result
