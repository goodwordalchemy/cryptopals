import Data.ByteString as B

type Message = B.ByteString

data Person { name :: String
            , keychain :: KeyChain
            } deriving (Show)

data KeyChain { privateKey :: String
              , publicKey :: String
              , base :: Int
              , modulus :: Int
              } deriving (Show)

echoBot :: Bool
