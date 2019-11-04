import Lib
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

plainText :: B.ByteString
plainText = Lib.stringToBytes "\
    \Burning 'em, if you ain't quick and nimble\n\
    \I go crazy when I hear a cymbal"

expectedCipherText :: B.ByteString
expectedCipherText = Lib.stringToBytes "\
    \0b3637272a2b2e63622c2e69692a23693a2a3c\
    \6324202d623d63343c2a26226324272765272a\
    \282b2f20430a652e2c652a3124333a653e2b20\
    \27630c692b20283165286326302e27282f"

key :: B.ByteString
key = Lib.stringToBytes "ICE"


main :: IO ()
main = do
    putStrLn "The should be equal..."
    putStr "expect: "
    BC.putStrLn expectedCipherText

    putStr "result: "
    BC.putStrLn $ Lib.bytesToHex $ Lib.repeatingXOR plainText key
