{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Fixed(Pico)
import Data.List(maximumBy)
import Network.HTTP.Simple
import System.TimeIt

import HmacServer(runServer)
import qualified Lib

baseUrl :: B.ByteString
baseUrl = "http://localhost:3000?file=fancyfilename&signature=" 

createRequest :: B.ByteString -> Request
createRequest signature = parseRequest_ 
                        $ BC.unpack
                        $ baseUrl `B.append` signature

timedResponse :: Request -> IO (Pico, Int)
timedResponse request = do
    !start <- Lib.getPicoSinceEpoch
    !response <- httpBS $ request
    !end <- Lib.getPicoSinceEpoch
    return $ (end-start, getResponseStatusCode response)
    

timeSignature :: B.ByteString -> IO Pico
timeSignature sig = liftM fst $ timedResponse $ createRequest sig

hexLetters :: [Char]
hexLetters = ['0'..'9'] ++ ['a'..'f']

getNextSignatures :: B.ByteString -> [B.ByteString]
getNextSignatures soFar = map addLetterAndPadding hexLetters
    where
        addLetterAndPadding l = (soFar `BC.snoc` l) `B.append` padding
        padding = BC.replicate nPadding 'A'
        nPadding = 40 - 1 - (B.length soFar)
        

maxBySnd :: Ord b => [(a, b)] -> a
maxBySnd = fst . maximumBy (\(_a, b) (_, b') -> compare b  b')


findHmac :: B.ByteString -> IO B.ByteString
findHmac soFar 
  | B.length soFar == 40 = return soFar
  | otherwise = do
      let sigs = getNextSignatures soFar
      !times <- sequence 
            $ map timeSignature sigs
      let bestSig = maxBySnd $ zip sigs times
          cur = fst $ B.splitAt (1 + B.length soFar) bestSig
      findHmac cur


test1 :: IO ()
test1 = do
    _ <- forkIO runServer

    let request = createRequest "f3477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    (requestTime, _) <- timeItT $ httpBS $ request
    print $ "request time: " ++ show requestTime

    let request' = createRequest "03477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    (requestTime', _) <- timeItT $ httpBS $ request'
    print $ "request time: " ++ show requestTime'

test2 :: IO ()
test2 = do
    _ <- forkIO runServer
    t <- timeSignature "fz3477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    t' <- timeSignature "f3z477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    t'' <- timeSignature "f34z77b6f15aef01a2f5b90df0da6c0ac619dfe71"
    t''' <- timeSignature "f347z7b6f15aef01a2f5b90df0da6c0ac619dfe71"
    print (t, t', t'', t''')

test3 :: IO ()
test3 = do
    _ <- forkIO runServer
    _ <- timeSignature "kickstart the server"
    result <- findHmac B.empty 
    print result

main :: IO ()
main = do
    test3
