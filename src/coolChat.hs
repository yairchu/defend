import Font
import Network
import Parse
import UI

import Control.Applicative ((<$))
import Control.Concurrent
import Control.Monad.Cont
import Control.Monad
import Data.List (intercalate)
import Data.List.Class (filter)
import Data.Map (lookup)
import Data.Monoid
import Data.Time.Clock (diffUTCTime)
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Backend.IO
import FRP.Peakachu.Backend.Time
import FRP.Peakachu.Internal
import Graphics.UI.GLUT
import Network.Socket
import Network.HTTP
import Text.Read.HT

import Prelude hiding (filter, lookup)

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
  font <- fmap loadFont $ readFile "../data/defend.font"
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
      (>>= mapM parseSockAddr . split sep) .
      filter (/= myAddrText)
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
      , mkSideEffect ((() <$) . mapM doPing) $ eMapMaybe id serverResponse
      , mkSideEffect doPong recvedPings
      , mkSideEffect print serverResponse
      , mkSideEffect print recvChars
      ]
    rChr (Character x) = Just x
    rChr _ = Nothing
    recvChars = eMapMaybe rChr $ fmap snd recvs
    gChr (KeyboardMouseEvent (Char x) Down _ _) = Just x
    gChr _ = Nothing
    snoc xs x = xs ++ [x]
    draw (now, msg) =
      Image $ do
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        renderPrimitive Triangles .
          forM_ msg $ \(charTime, char) -> do
            let
              timeDiff = realToFrac $ diffUTCTime now charTime
              polys =
                maybe [] (concat . pixBody) $
                lookup [char] font
            color $ Color4 1 0.5 0.25 (1 - abs (0.5+timeDiff/2-1))
            forM_ polys $ \(x, y) ->
              vertex $ Vertex4 x y 0 (max 1 (3-timeDiff))
    prog ui =
      (image, effects)
      where
        image =
          fmap draw .
          eZipByFst (drawingTime 0.1 ui) .
          escanl snoc [] $ zipTime recvChars
        effects = mconcat
          [ nonUiEffects
          , mkSideEffect (uncurry sendChar) $
            eZipByFst (eMapMaybe gChr ui) peers
          ]
  initialWindowSize $= Size 512 512
  run prog

