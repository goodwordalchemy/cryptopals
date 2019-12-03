{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HmacServer() where
import Network.HTTP.Types.Status(status500)
import Yesod

data HMacForFile = HMacForFile

mkYesod "HMacForFile" [parseRoutes|
/ HomeR GET
|]

instance Yesod HMacForFile

getHomeR :: Handler Html
getHomeR = do
    qMaybe <- lookupGetParam "Q"
    case qMaybe of
      Just q -> defaultLayout [whamlet|Hello World! #{q}|] 
      Nothing -> do 
          html <- defaultLayout [whamlet|Yo, where's the Q?|] 
          sendResponseStatus status500 html

main :: IO ()
main = warp 3000 HMacForFile
