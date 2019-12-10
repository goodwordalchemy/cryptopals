{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.ByteString as B

import DiffieHellman( genPublicKey
                    , getCommonKey
                    ) 
import qualified Lib

data Person  =  Person
                { _name :: String
                , _keyChain :: KeyChain
                , _inbound :: Message
                , _outbound :: Message
    
                } deriving (Show)

type PP = (Person, Person)

data KeyChain = KeyChain
                { _privateKey :: Integer
                , _publicKey :: Maybe Integer
                , _modulus :: Maybe Integer
                , _commonKey :: Maybe Integer
                } deriving (Show)

data Message = Invitation 
                { _publicKey' :: Integer
                , _base :: Integer
                , _modulus' :: Integer
                } 
             | Acceptance
                { _publicKey' :: Integer }
             | Info
                { _content :: B.ByteString
                , _iv :: B.ByteString
                } 
             | Quiet
               deriving (Show)

$(makeLenses ''Person)
$(makeLenses ''KeyChain)
$(makeLenses ''Message)

makePerson :: String -> Integer -> Person
makePerson name privateKey = 
    let kc = KeyChain 
                { _privateKey = privateKey
                , _publicKey = Nothing
                , _modulus = Nothing
                , _commonKey = Nothing
                }
    in Person 
        { _name = name
        , _keyChain = kc
        , _inbound = Quiet
        , _outbound = Quiet
        }

updateWithBaseAndModulus :: Integer -> Integer -> Person -> Person
updateWithBaseAndModulus base_ modulus_ person =
    let priv = view (keyChain.privateKey) person
        pub = genPublicKey base_ priv modulus_
        
        person' = set (keyChain.publicKey) (Just pub) person
        person'' = set (keyChain.modulus) (Just modulus_) person'

    in person''

sendMessage :: PP -> PP
sendMessage (a, b) = 
    let b' = set inbound (view outbound a) b
        a' = set outbound Quiet a
    in (a', b')

sendInvitation :: Integer -> Integer -> PP -> PP
sendInvitation base_ modulus_ (a, b) =
    let Just aPub = view (keyChain.publicKey) a
        msg = Invitation aPub base_ modulus_
        a' = set outbound msg a
    in sendMessage (a', b)

receiveInvitation :: Person -> Person
receiveInvitation b = 
    let g = view (inbound.base) b
        p = view (inbound.modulus') b
        b' = updateWithBaseAndModulus g p b

        aPub = view (inbound.publicKey') b'
        bPriv = view (keyChain.privateKey) b'
        Just modulus_ = view (keyChain.modulus) b'
        commonKey_ = getCommonKey aPub bPriv modulus_

        b'' = set (keyChain.commonKey) (Just commonKey_) b'

    in b''

sendAcceptance :: PP -> PP
sendAcceptance (a, b) = 
    let (Just bPub) = view (keyChain.publicKey) b
        msg = Acceptance bPub
        b' = set outbound msg b
    in sendMessage (a, b')

receiveAcceptance :: Person -> Person
receiveAcceptance a =
    let bPub = view (inbound.publicKey') a
        aPriv = view (keyChain.privateKey) a
        Just modulus_ = view (keyChain.modulus) a
        commonKey_ = getCommonKey bPub aPriv modulus_
        
        a' = set (keyChain.commonKey) (Just commonKey_) a

    in a'

commonAESKey :: Person -> B.ByteString
commonAESKey p = 
    let (Just key) = view (keyChain.commonKey) p 
    in Lib.strictSha1 . Lib.littleEndian64 . fromInteger $ key

prepareEncryptedMessage 
    :: B.ByteString -> B.ByteString -> Person -> Person
prepareEncryptedMessage info iv_ p =
    let aes = Lib.initAES128 (commonAESKey p)
        content_ = Lib.cbcEncryption aes iv_ info
        msg = Info content_ iv_

        p' = set outbound msg p

    in p'

sendEncryptedMessage :: B.ByteString -> B.ByteString -> PP -> PP
sendEncryptedMessage info iv_ (a, b) = 
    let a' = prepareEncryptedMessage info iv_ a
    in sendMessage (a', b)

receiveEncryptedMessage :: Person -> B.ByteString
receiveEncryptedMessage p =
    let content_ = view (inbound.content) p
        iv_ = view (inbound.iv) p

        aes = Lib.initAES128 (commonAESKey p) 
        info = Lib.cbcDecryption aes iv_ content_
    in info
    
echoBot :: Integer -> Integer -> Integer -> Integer -> Bool
echoBot aPriv bPriv g p = 
    let a = makePerson "Alice" aPriv
        b = makePerson "Bob" bPriv
        (a', b') = sendInvitation g p (a, b)
        b'' = receiveInvitation b
        (a'', b''') = sendAcceptance (a', b'')
        (a''', b'''') = 
            sendEncryptedMessage 
                "hello with my message" 
                "YELLOW SUBMARINE" 
                (a'', b''')
        mAB = receiveEncryptedMessage b''''
        (b''''', a'''') = 
            sendEncryptedMessage 
                mAB
                "MELLOW SUBMARINE" 
                (b'''', a''')
        mBA = receiveEncryptedMessage a''''
     in mAB == mBA

main :: IO ()
main = print $ echoBot 1 2 3 4
        
