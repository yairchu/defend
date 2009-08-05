module Network (
  listenUdpExternal, resolveDomainName, stunService
  ) where

import ParseStun (
  StunMessage(..),
  parseStun, getRealMappedAddress)

import Control.Monad (when, replicateM)
import Data.Char (chr)
import Data.Maybe (mapMaybe)
import Network.Socket (
  Family(..), ServiceName,
  SockAddr(..), Socket, SocketType(..),
  addrAddress, getAddrInfo, recvFrom, sendTo, socket)
import Random (randomRIO)

stunService :: ServiceName
stunService = "3478"

resolveDomainName :: String -> ServiceName -> IO SockAddr
resolveDomainName hostName serviceName =
  fmap (addrAddress . head) $
  getAddrInfo Nothing (Just hostName) (Just serviceName)

-- | Opens a UDP socket
-- and finds out its external ip and port
-- using the service of a STUN server
listenUdpExternal :: SockAddr -> IO (Socket, SockAddr)
listenUdpExternal stunServer = do
  requestRaw <-
    fmap ("\0\1\0\0" ++) .
    replicateM 16 . fmap chr $ randomRIO (0, 255)
  sock <- socket AF_INET Datagram 0
  sendTo sock requestRaw stunServer
  (responseRaw, _, _) <- recvFrom sock 1024
  let
    Just request = parseStun requestRaw
    Just response = parseStun responseRaw
    Just address = getRealMappedAddress response
  when (stunMsgType response /= 0x101) $
    fail "wrong response type"
  when (stunTransactId response /= stunTransactId request) $
    fail "wrong transaction id"
  return (sock, address)

