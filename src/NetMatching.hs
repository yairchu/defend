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

netMatching
  :: PeakaSocket
  -> EffectFunc String (Maybe String) ()
  -> EffectFunc () () ()
  -> Event Bool
  -> (Event SockAddr, SideEffect)
netMatching sock http timer isConnected =
  ((eFlatten . eMapMaybe id) serverResponse, effects)
  where
    effects = mconcat
      [ efRun http $ (url, ()) <$ ereturn () `merge` retries
      , efRun timer $ ((), ()) <$ serverResponse
      ]
    urlPrefix = "http://defendtheking.nfshost.com/match.cgi?"
    url = urlPrefix ++ myAddrText
    myAddrText = intercalate "," . map show . psAddresses $ sock
    retries = () <$ efilter (not . snd) (eZipByFst (efOut timer) isConnected)
    serverResponse :: Event (Maybe [SockAddr])
    serverResponse = parseDifferentAddresses . fst <$> efOut http
    parseDifferentAddresses =
      (>>= mapM parseSockAddr . split ',') .
      filter (/= myAddrText)

