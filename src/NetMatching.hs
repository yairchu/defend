{-# LANGUAGE TemplateHaskell #-}

module NetMatching where

import Networking
import Parse

import Control.Category
import Control.FilterCategory
import Control.Monad (guard)
import Data.ADT.Getters
import Data.List (intercalate)
import Data.Map (delete, insert, lookup)
import Data.Monoid (Monoid(..))
import FRP.Peakachu.Program
import Network.Socket (SockAddr)
import Prelude hiding ((.), id, lookup)

data MatchingIn a
  = DoMatching [SockAddr] a
  | MITimerEvent a
  | MIHttp (Maybe String) a

data MatchingOut a
  = MatchingResult [SockAddr] a
  | MOSetRetryTimer a
  | MOHttp String a
  deriving Show
$(mkADTGetters ''MatchingOut)

netMatching :: Ord a => Program (MatchingIn a) (MatchingOut a)
netMatching =
  mapMaybeC snd . scanlP step (mempty, Nothing)
  where
    step (state, _) (DoMatching addrs tag) =
      (newState, req newState tag)
      where
        newState = insert tag addrs state
    step (state, _) (MIHttp response tag) =
      case parseDifferentAddresses state tag response of
      Nothing ->
        ( state
        , Just $ MOSetRetryTimer tag
        )
      Just res ->
        ( delete tag state
        , Just $ MatchingResult res tag
        )
    step (state, _) (MITimerEvent tag) = (state, req state tag)
    req state tag =
      fmap
        ( (`MOHttp` tag)
        . ("http://defendtheking.nfshost.com/match.cgi?" ++)
        )
      . formatAddrs state $ tag
    formatAddrs state =
      fmap (intercalate "," . map show) . (`lookup` state)
    parseDifferentAddresses state tag response = do
      text <- response
      myAddrText <- formatAddrs state tag
      guard $ myAddrText /= text
      mapM parseSockAddr . split ',' $ text

