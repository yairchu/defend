import Network
import Parse

import Control.Applicative ((<$))
import Control.Concurrent
import Control.Monad.Cont
import Control.Monad
import Data.List (intercalate)
import Data.List.Class (filter)
import Data.Monoid
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Backend.IO
import FRP.Peakachu.Internal
import Graphics.UI.GLUT
import Network.Socket
import Network.HTTP
import Text.Read.HT

import Prelude hiding (filter)

-- more options at http://www.voip-info.org/wiki/view/STUN
stunServer :: String
stunServer = "stun.ekiga.net"

httpGet :: IO (EffectfulFunc String (Maybe String) a)
httpGet =
  mkEffectfulFunc go
  where
    go uri = do
      liftForkIO
      eresp <- lift . simpleHTTP $ getRequest uri
      ContT $ case eresp of
        Left _ -> ($ Nothing)
        Right resp -> ($ Just (rspBody resp))

recvFromE :: Socket -> Int -> IO (Event (String, Int, SockAddr))
recvFromE sock size = do
  (event, callback) <- makeCallbackEvent
  forkIO . forever $ recvFrom sock size >>= callback
  return event

data Message = Ping | Pong | Character Char
  deriving (Eq, Read, Show)

main :: IO ()
main = do
  (sock, myAddresses) <-
    getHostAddrByName stunServer >>=
    createListenUdpSocket . SockAddrInet stunPort
  print myAddresses
  (getHttp, gotHttp) <- httpGet
  (setRetryTimer, retryTimer) <- setTimerEvent
  recvsRaw <- recvFromE sock 1024
  let
    sep = ','
    myAddrText = intercalate [sep] $ map show myAddresses
    parseDifferentAddresses =
      (>>= sequence . map parseSockAddr . split sep) .
      filter (/= myAddrText) .
      fmap (takeWhile (/= '\n'))
    urlPrefix = "http://defendtheking.nfshost.com/match.cgi?"
    url = urlPrefix ++ myAddrText
    retryInterval = 2000
    serverResponse :: Event (Maybe [SockAddr])
    serverResponse = fmap (parseDifferentAddresses . fst) gotHttp
    recvs = eMapMaybe fmsg recvsRaw
    recvedPings = efilter ((== Ping) . snd) recvs
    fmsg (msgText, _, address) = fmap ((,) address) (maybeRead msgText)
    gotPong = ereturn False `merge` (True <$ efilter ((== Pong) . snd) recvs)
    retries = () <$ efilter (not . snd) (eZipByFst retryTimer gotPong)
    doPing address = do
      putStrLn $ "sendPing " ++ show address
      sendTo sock (show Ping) address
      return ()
    doPong (address, _) = do
      putStrLn $ "sendPong " ++ show address
      sendTo sock (show Pong) address
      return ()
    peers = escanl (flip (:)) [] $ fmap fst recvedPings
    sendChar = mapM_ . sendTo sock . show . Character
    nonUiEffects = mconcat
      [ getHttp $ (url, ()) <$ ereturn () `merge` retries
      , setRetryTimer $ (retryInterval, ()) <$ serverResponse
      , mkSideEffect ((() <$) . sequence . map doPing) $ eMapMaybe id serverResponse
      , mkSideEffect doPong recvedPings
      , mkSideEffect print serverResponse
      , mkSideEffect print recvChars
      ]
    rChr (Character x) = Just x
    rChr _ = Nothing
    recvChars = eMapMaybe rChr $ fmap snd recvs
    gChr (KeyboardMouseEvent (Char x) Down _ _) = Just x
    gChr _ = Nothing
    prog ui =
      (empty, effects)
      where
        effects = mconcat
          [ nonUiEffects
          , mkSideEffect (uncurry sendChar) $
            eZipByFst (eMapMaybe gChr ui) peers
          ]
  initialWindowSize $= Size 512 512
  run prog

