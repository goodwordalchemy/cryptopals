{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HmacServer() where
import Network.HTTP.Types.Status(status400, status500)
import Yesod

data HMacForFile = HMacForFile

mkYesod "HMacForFile" [parseRoutes|
/ HomeR GET
|]

instance Yesod HMacForFile

badRequest :: Handler Html
badRequest = do 
    html <- defaultLayout [whamlet|Bad request|] 
    sendResponseStatus status400 html

getHomeR :: Handler Html
getHomeR = do
    fileMaybe <- lookupGetParam "file"
    case fileMaybe of
        Nothing -> badRequest
        Just file -> do
           signatureMaybe <- lookupGetParam "signature"
           case signatureMaybe of
               Nothing -> badRequest
               Just signature -> defaultLayout [whamlet|file: #{file}, signature: #{signature}|] 

main :: IO ()
main = warp 3000 HMacForFile
