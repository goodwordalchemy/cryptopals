module Challenge1  ( challenge1
                   , base64Output
) where

import Lib

hexToBase64 :: String -> String
hexToBase64 = 
    Lib.bytesToString . Lib.bytesToBase64 . Lib.hexToBytes . Lib.stringToBytes 

challenge1 :: String
challenge1 = hexToBase64 hexInput
    
hexInput :: String
hexInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

base64Output :: String
base64Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

main :: IO ()
main = do
    putStrLn "The should be equal..."
    putStr "expect: "
    putStrLn base64Output

    putStr "result: "
    putStrLn $ challenge1
