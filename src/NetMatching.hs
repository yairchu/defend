module NetMatching where

import Networking
import Parse

import Control.Applicative
import Data.List (intercalate)
import Data.List.Class (filter)
import Data.Monoid (mconcat)
import FRP.Peakachu
import Network.Socket (SockAddr)
import Prelude hiding (filter)

netMatching :: PeakaSocket -> EffectfulFunc String (Maybe String) () -> EffectfulFunc () () () -> Event Bool -> (Event SockAddr, SideEffect)
netMatching sock (getHttp, gotHttp) (setRetryTimer, retryTimer) isConnected =
  ((eFlatten . eMapMaybe id) serverResponse, effects)
  where
    effects = mconcat
      [ getHttp $ (url, ()) <$ ereturn () `merge` retries
      , setRetryTimer $ ((), ()) <$ serverResponse
      ]
    urlPrefix = "http://defendtheking.nfshost.com/match.cgi?"
    url = urlPrefix ++ myAddrText
    myAddrText = intercalate "," . map show . psAddresses $ sock
    retries = () <$ efilter (not . snd) (eZipByFst retryTimer isConnected)
    serverResponse :: Event (Maybe [SockAddr])
    serverResponse = fmap (parseDifferentAddresses . fst) gotHttp
    parseDifferentAddresses =
      (>>= mapM parseSockAddr . split ',') .
      filter (/= myAddrText)

