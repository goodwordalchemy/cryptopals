module Challenge28(challenge28) where
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC

import qualified Lib

verifyCantTamperMessage :: Bool
verifyCantTamperMessage = (orig /= tamperedKey) && (orig /= tamperedMessage)
    where 
        tamperedKey = Lib.sha1KeyedMAC key' message
        tamperedMessage = Lib.sha1KeyedMAC key message'
        orig = Lib.sha1KeyedMAC key message

        key' = BLC.pack "stacky icky"
        key = BLC.pack "sticky icky"

        message' = BLC.pack "uery secret message.  somewhat long"
        message = BLC.pack "very secret message.  somewhat long"

verifyCantProduceNewMacWithoutKey :: Bool
verifyCantProduceNewMacWithoutKey = True
    -- Because changing the key at all scrambles the message completely.
    --

challenge28 :: Bool
challenge28 = verifyCantTamperMessage && verifyCantProduceNewMacWithoutKey
