module Networking (
  bindUdpAnyPort, createListenUdpSocket,
  getHostAddrByName, parseSockAddr, stunPort
  ) where

import Parse (split)
import ParseStun (
  StunMessage(..),
  parseStun, getRealMappedAddress)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Concurrent.MVar.YC (writeMVar)
import Control.Monad (join, liftM2, unless, when, replicateM)
import Data.Char (chr)
import Data.Function (fix)
import Data.List (nub)
import Network.BSD (getHostByName, getHostName, hostAddress)
import Network.Socket (
  Family(..), HostAddress, PortNumber(..),
  SockAddr(..), Socket, SocketType(..),
  bindSocket, iNADDR_ANY,
  recvFrom, sendTo, socket
  )
import Random (randomRIO)
import System.IO.Error (isAlreadyInUseError, try)
import Text.Read.HT (maybeRead)

stunPort :: PortNumber
stunPort = fromInteger 3478

maybeIO :: (IOError -> Bool) -> IO a -> IO (Maybe a)
maybeIO isExpected =
  join . fmap f . try
  where
    f (Right x) = return $ Just x
    f (Left err)
      | isExpected err = return Nothing
      | otherwise = ioError err

bindUdpAnyPort :: Socket -> IO PortNumber
bindUdpAnyPort sock = do
  portNum <- fmap (PortNum . fromIntegral) $ randomRIO (0x8000 :: Int, 0xFFFF)
  r <- maybeIO isAlreadyInUseError .
    bindSocket sock $
    SockAddrInet portNum iNADDR_ANY
  case r of
    Nothing -> bindUdpAnyPort sock
    _ -> return portNum

getHostAddrByName :: String -> IO HostAddress
getHostAddrByName =
  fmap hostAddress . getHostByName

getHostAddress :: IO HostAddress
getHostAddress =
  getHostName >>= getHostAddrByName

-- | Find out internet address outside of NAT
-- using a stun server
udpGetInternetAddr :: SockAddr -> Socket -> IO SockAddr
udpGetInternetAddr stunServer sock = do
  gotResponseVar <- newMVar False
  forkIO . fix $ \resume -> do
    gotResponse <- readMVar gotResponseVar
    unless gotResponse $ do
      requestRaw <-
        fmap ("\0\1\0\0" ++) .
        replicateM 16 . fmap chr $ randomRIO (0, 255)
      sendTo sock requestRaw stunServer
      threadDelay 500000 -- 0.5 second
      resume
  (responseRaw, _, _) <- recvFrom sock 1024
  writeMVar gotResponseVar True
  let
    Just response = parseStun responseRaw
    Just address = getRealMappedAddress response
  when (stunMsgType response /= 0x101) $
    fail "wrong response type"
  return address

createListenUdpSocket :: SockAddr -> IO (Socket, [SockAddr])
createListenUdpSocket stunServer = do
  sock <- socket AF_INET Datagram 0
  iAddr <- liftM2 SockAddrInet (bindUdpAnyPort sock) getHostAddress
  eAddr <- udpGetInternetAddr stunServer sock
  return (sock, nub [iAddr, eAddr])

parseSockAddr :: String -> Maybe SockAddr
parseSockAddr text = do
  prt <- maybeRead portText :: Maybe Int
  ipBytes <- mapM maybeRead ipBytesText
  let hst = foldl ((+) . (* 0x100)) 0 (reverse ipBytes)
  return $ SockAddrInet (fromIntegral prt) hst
  where
    (ipText, portText') = break (== ':') text
    portText = drop 1 portText'
    ipBytesText = split '.' ipText

