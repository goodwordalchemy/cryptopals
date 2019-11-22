padLastBlock :: Int -> B.ByteString -> B.ByteString
padLastBlock len block = padded
    where
        padded = B.concat [ block
                          , B.singleton 0x80
                          , encodedLength
                          ]
        zeros = B.replicate (56 - (len `mod` 64)) 0
        encodedLength = Lib.bigEndian len

sha1MdPadHelper :: Int-> B.ByteString -> B.ByteString
sha1MdPadHelper len text 
  | B.length text <= 56 = padLastBlock text
  | otherwise = B.append front $ sha1MdPadHelper len back
    where
        (front, back) = B.split 64 text


sha1MdPad :: B.ByteString -> B.ByteString
sha1MdPad text = sha1MdPadHelper (B.length text) text
