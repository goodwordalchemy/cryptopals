{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent(forkIO)
import Control.Monad(liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple
import System.TimeIt

import HmacServer(runServer)

baseUrl :: String
baseUrl = "http://localhost:3000?file=fancyfilename&signature=" 

createRequest :: String -> Request
createRequest signature = parseRequest_ $ baseUrl ++ signature

timedResponse :: Request -> IO (Double, Response B.ByteString)
timedResponse request = timeItT $ httpBS $ request

timeSignature :: String -> IO Double
timeSignature sig = liftM fst $ timedResponse $ createRequest sig

-- findNextLetter :: B.ByteString -> 

test1 :: IO ()
test1 = do
    forkIO runServer

    let request = createRequest "f3477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    (requestTime, response) <- timeItT $ httpBS $ request
    print $ "request time: " ++ show requestTime

    let request' = createRequest "03477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    (requestTime', response') <- timeItT $ httpBS $ request'
    print $ "request time: " ++ show requestTime'

main :: IO ()
main = do
    forkIO runServer
    t <- timeSignature "f3477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    print t 
