{-# LANGUAGE TemplateHaskell #-}

module NetMatching where

import Networking
import Parse

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Data.ADT.Getters
import Data.List (intercalate)
import Data.List.Class (filter)
import Data.Map (Map, (!), delete, insert)
import Data.Monoid (Monoid(..), mconcat)
import FRP.Peakachu.Program
import Network.Socket (SockAddr)
import Prelude hiding ((.), id, filter)

data MatchingIn a
  = DoMatching [SockAddr] a
  | MITimerEvent a
  | MIHttp (Maybe String) a

data MatchingOut a
  = MatchingResult [SockAddr] a
  | MOSetRetryTimer a
  | MOHttp String a
$(mkADTGetterCats ''MatchingOut)

data MatchingMid a
  = MI (MatchingIn a)
  | MState (Map a [SockAddr])
$(mkADTGetterCats ''MatchingMid)

netMatching :: Ord a => Program (MatchingIn a) (MatchingOut a)
netMatching =
  rid
  . (f <$> cMState <*> cMI)
  . mconcat
  [ MState <$> scanlP step mempty
  , arr MI
  ]
  where
    f state (DoMatching _ tag) = req state tag
    f state (MITimerEvent tag) = req state tag
    f state (MIHttp response tag) =
      case parseDifferentAddresses state tag response of
      Nothing -> Just $ MOSetRetryTimer tag
      Just res -> Just $ MatchingResult res tag
    f _ _ = Nothing
    step state (DoMatching addrs tag) =
      insert tag addrs state
    step state (MIHttp response tag) =
      case parseDifferentAddresses state tag response of
      Nothing -> state
      _ -> delete tag state
    step state _ = state
    req state tag =
      Just . (`MOHttp` tag)
      . ("http://defendtheking.nfshost.com/match.cgi?" ++)
      . formatAddrs state $ tag
    formatAddrs state =
      intercalate "," . map show . (state !)
    parseDifferentAddresses state tag =
      (>>= mapM parseSockAddr . split ',') .
      filter (/= formatAddrs state tag)

