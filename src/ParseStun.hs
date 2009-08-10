-- | STUN (Simple Traversal for UDP through NAT)
-- is a protocol that allows to discover the external addresses
-- of UDP sockets one initiates.
--
-- It requires a public STUN server (see list of those at "http://www.voip-info.org/wiki/view/STUN")

module ParseStun (
  StunAttribute(..), StunMessage(..),
  parseStun, getRealMappedAddress
  ) where

import Control.Monad (guard, forever, msum, mzero, when)
import Control.Monad.Maybe (runMaybeT)
import Control.Monad.Writer (execWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Binary.Get (
  Get, bytesRead, getLazyByteString,
  getWord8, getWord16be, getWord16le, getWord32be, getWord32le,
  remaining, runGet, skip)
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString, pack, unpack)
import Data.ByteString.Internal (c2w, w2c) -- must I really resort to use an "Internal" module?
import Data.Word (Word16, Word32)
import Network.Socket (PortNumber(..), SockAddr(..))

-- There must be something built in for these:
string2ByteString :: String -> ByteString
string2ByteString = pack . map c2w
byteString2String :: ByteString -> String
byteString2String = map w2c . unpack

getString :: Integral i => i -> Get String
getString =
  fmap byteString2String .
  getLazyByteString .
  fromIntegral

data StunAttribute
  = StunMappedAddress SockAddr
  | StunResponseAddress SockAddr
  | StunSourceAddress SockAddr
  | StunChangedAddress SockAddr
  -- XOR_MAPPED_ADDRESS is a work-around for the behaviour
  -- of Linksys routers which performs man-in-the-middle
  -- "attacks" to modify the MAPPED_ADDRESS.
  -- Not sure why Linksys does this.
  | StunXorMappedAddress SockAddr
  | StunServer String
  | StunAttribute Word16 String 
  deriving Show

data StunMessage = StunMessage {
  stunMsgType :: Word16,
  stunMagicCookie :: Word32,
  stunTransactId :: String,
  stunAttributes :: [StunAttribute]
} deriving Show

parseStun :: String -> Maybe StunMessage
parseStun src =
  (`runGet` string2ByteString src) . runMaybeT $ do
    msgType <- lift getWord16be
    msgSize <- lift getWord16be
    magic <- lift getWord32be
    transactId <- lift $ getString (12::Int)
    stopPos <- fmap (+ fromIntegral msgSize) $ lift bytesRead
    attributes <-
      execWriterT .
      -- MaybeT to break from loop
      -- the inner MaybeT is for parsing failure
      runMaybeT .
      forever $ do
        let
          liftGet = lift . lift . lift
          failure = lift $ lift mzero
          failIf = (`when` failure)
        guard . (< stopPos) =<< liftGet bytesRead
        attrType <- liftGet getWord16be
        attrSize <- liftGet getWord16be
        let
          readAddress = do
            failIf $ 8 /= attrSize
            liftGet $ skip 1
            adFamily <- liftGet getWord8
            failIf $ 1 /= adFamily
            adPort <- liftGet getWord16le
            adHost <- liftGet getWord32le
            return $ SockAddrInet (PortNum adPort) adHost
          readString = liftGet $ getString attrSize
        tell . return =<< case attrType of
          1 -> fmap StunMappedAddress readAddress
          2 -> fmap StunResponseAddress readAddress
          4 -> fmap StunSourceAddress readAddress
          5 -> fmap StunChangedAddress readAddress
          0x8020 -> fmap StunXorMappedAddress readAddress
          0x8022 -> fmap StunServer readString
          _ -> fmap (StunAttribute attrType) readString
        failIf . (> stopPos) =<< liftGet bytesRead
    (`when` mzero) . (/= 0) =<< lift remaining
    return $ StunMessage msgType magic transactId attributes

littleEndianDigits :: Integral i => i -> i -> [i]
littleEndianDigits base =
  map (`mod` base) . iterate (`div` base)

fromBigEndianDigits :: Integral i => i -> [i] -> i
fromBigEndianDigits base =
  foldl step 0
  where
    step res x = res * base + x

flipEndian :: Integral i => Int -> i -> i
flipEndian numBytes =
  fromBigEndianDigits 256 .
  take numBytes .
  littleEndianDigits 256

getRealMappedAddress :: StunMessage -> Maybe SockAddr
getRealMappedAddress msg =
  -- msum of [Maybe a] takes the first Just
  msum $ map fXor attrs ++ map fMap attrs
  where
    attrs = stunAttributes msg
    magic = stunMagicCookie msg
    fXor (StunXorMappedAddress (SockAddrInet (PortNum xPort) xHost)) =
      Just $ SockAddrInet (PortNum port) host
      where
        port = xPort `xor` flipEndian 2 (fromIntegral (magic `div` 0x10000))
        host = xHost `xor` flipEndian 4 magic
    fXor _ = Nothing
    fMap (StunMappedAddress x) = Just x
    fMap _ = Nothing

