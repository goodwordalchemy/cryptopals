import Data.ByteString as B

type Message = B.ByteString

data Person { name :: String
            , privateKey :: String 
            , keyChain :: Maybe KeyChain
            } deriving (Show)

data KeyChain = {
                , base :: Int
                , modulus :: Int
                , publicKey :: String
                } deriving (Show)

echoBot :: Bool
echoBot aPriv bPriv = 
    let a = Person "Alice" aPriv Nothing
        b = Person "Bob" bPriv Nothing
