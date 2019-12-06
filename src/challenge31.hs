{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Challenge31(
    nextByte, findHmac, NextByteFunc
) where

import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Fixed(Pico)
import Data.List(maximumBy)
import Debug.Trace
import Network.HTTP.Simple
import System.TimeIt

import HmacServer(runServer)
import qualified Lib

baseUrl :: B.ByteString
baseUrl = "http://localhost:3000?file=fancyfilename&signature=" 

createRequest :: B.ByteString -> Request
createRequest signature = parseRequest_ 
                        $ BC.unpack
                        $ baseUrl `B.append` hexSig
    where
        hexSig = Lib.bytesToHex signature

timedResponse :: Request -> IO (Pico, Int)
timedResponse request = do
    !start <- Lib.getPicoSinceEpoch
    !response <- httpBS $ request
    !end <- Lib.getPicoSinceEpoch
    return $ (end-start, getResponseStatusCode response)
    

timeSignature :: B.ByteString -> IO Pico
timeSignature sig = liftM fst $ timedResponse $ createRequest sig

getNextSignatures :: B.ByteString -> [B.ByteString]
getNextSignatures soFar = map addLetterAndPadding [0..255]
    where
        addLetterAndPadding l = 
            B.concat [ soFar 
                     , B.singleton l
                     , padding
                     ]
        padding = BC.replicate nPadding 'A'
        nPadding = 20 - 1 - (B.length soFar)
        

maxBySnd :: Ord b => [(a, b)] -> a
maxBySnd = fst . maximumBy (\(_a, b) (_, b') -> compare b  b')

type NextByteFunc = (Float, Float) -> Int -> B.ByteString -> IO (Maybe B.ByteString)

nextByte :: NextByteFunc
nextByte (a,b) delayTime soFar = go $ getNextSignatures soFar
    where
        go [] = return Nothing
        go (s:ss) = do
            time <- timeSignature s
            (trace $ "time: " ++ show time ++ ", cutoff: " ++ show cutoff) $ 
                if realToFrac time > cutoff
                   then return $ Just $ extractCur s
                   else go ss

        cutoff = expectedDelay + (a * expectedDelay) + (b * delayTime')
        expectedDelay = delayTime' * (fromIntegral $ B.length soFar)
        delayTime' = (fromIntegral delayTime) / (10^6)

        extractCur = fst . B.splitAt (1 + B.length soFar)
        

backtrack :: B.ByteString -> B.ByteString
backtrack b = (trace $ "backtracking " ++ show b)
            $ fst
            $ B.splitAt (B.length b - 2) b

findHmac 
    :: NextByteFunc -> (Float, Float) -> Int -> B.ByteString -> IO B.ByteString
findHmac nextByteFunc coefs delayTime soFar 
  | B.length soFar == 20 = return soFar
  | otherwise = do
      maybeCur <- nextByteFunc coefs delayTime soFar
      case maybeCur of
        Just cur -> findHmac nextByteFunc coefs delayTime cur
        Nothing -> findHmac nextByteFunc coefs delayTime $ backtrack soFar

delayTime :: Int 
delayTime = 50 * 1000

findHmac50ms :: B.ByteString -> IO B.ByteString
findHmac50ms = findHmac nextByte (0.05, 0.9) delayTime


test1 :: IO ()
test1 = do
    _ <- forkIO $ runServer delayTime

    let request = createRequest "f3477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    (requestTime, _) <- timeItT $ httpBS $ request
    print $ "request time: " ++ show requestTime

    let request' = createRequest "03477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    (requestTime', _) <- timeItT $ httpBS $ request'
    print $ "request time: " ++ show requestTime'

test2 :: IO ()
test2 = do
    _ <- forkIO $ runServer delayTime
    t <- timeSignature "fz3477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    t' <- timeSignature "f3z477b6f15aef01a2f5b90df0da6c0ac619dfe71"
    t'' <- timeSignature "f34z77b6f15aef01a2f5b90df0da6c0ac619dfe71"
    t''' <- timeSignature "f347z7b6f15aef01a2f5b90df0da6c0ac619dfe71"
    print (t, t', t'', t''')

test3 :: IO ()
test3 = do
    _ <- forkIO $ runServer delayTime
    -- result <- findHmac50ms B.empty 
    result <- findHmac50ms $ Lib.hexToBytes "f3477b6f15aef01a2f5b90df"
    print result

main :: IO ()
main = do
    test3
