filename :: String
filename = "data/6.txt"

loadEncryptedFile :: IO B.ByteString
loadEncryptedFile = do
    contents <- B.readFile filename
    return $ Lib.base64ToBytes contents
