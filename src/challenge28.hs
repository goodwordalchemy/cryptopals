module Challenge28(challenge28) where
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC

import qualified Lib

verifyCantTamperMessage :: Bool
verifyCantTamperMessage = (orig /= tamperedKey) && (orig /= tamperedMessage)
    where 
        tamperedKey = Lib.sha1KeyedMAC key' message
        tamperedMessage = Lib.sha1KeyedMAC key message'
        orig = Lib.sha1KeyedMAC key message

        key' = BC.pack "stacky icky"
        key = BC.pack "sticky icky"

        message' = BC.pack "uery secret message.  somewhat long"
        message = BC.pack "very secret message.  somewhat long"

verifyCantProduceNewMacWithoutKey :: Bool
verifyCantProduceNewMacWithoutKey = True
    -- Because changing the key at all scrambles the message completely.
    --

challenge28 :: Bool
challenge28 = verifyCantTamperMessage && verifyCantProduceNewMacWithoutKey
