{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent(forkIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple

import HmacServer(runServer)

main :: IO ()
main = do
