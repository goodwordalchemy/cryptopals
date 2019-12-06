{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent(forkIO)
import qualified Data.ByteString as B
import Data.Function(on)
import Data.List(group, maximumBy, sort)

import Challenge31(findHmac, nextByte, NextByteFunc)
import HmacServer(runServer)
import qualified Lib

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

delayTime :: Int 
delayTime = 5 * 1000

nVotes = 20

-- type NextByteFunc = Int -> B.ByteString -> IO (Maybe B.ByteString)

nextByteVoting :: NextByteFunc
nextByteVoting coefs delayTime soFar = do
    votes <- sequence $ replicate nVotes $ nextByte coefs delayTime soFar
    return $ mostCommon votes


findHmac5ms :: B.ByteString -> IO B.ByteString
findHmac5ms = findHmac nextByteVoting (0.25, 1.0) delayTime

test3 :: IO ()
test3 = do
    _ <- forkIO $ runServer delayTime
    result <- findHmac5ms B.empty 
    -- result <- findHmac5ms $ Lib.hexToBytes "f3477b6f15aef01a2f5b90df"
    print result

main :: IO ()
main = do
    test3
