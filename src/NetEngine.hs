module NetEngine where

import Control.Applicative ((<$), (<$>))
import Data.Map (Map, delete, fromList, insert, lookup)
import Data.Monoid (Monoid(..))
import FRP.Peakachu (
  EffectfulFunc, Event, SideEffect,
  eMapMaybe, ereturn, escanl, merge)
import Prelude hiding (lookup)

data NetEngine moveType idType = NetEngine {
  neLocalMove :: moveType,
  neQueue :: Map (Integer, idType) moveType,
  neLatencyIters :: Integer,
  nePeerId :: idType,
  nePeers :: [idType],
  neWaitingForPeers :: Bool,
  neGameIteration :: Integer,
  neOutput :: Maybe moveType
  }

data NetEngInput a
  = LocalMove (a -> a)
  | IterTimer

netEngineStep ::
  (Monoid a, Ord i) =>
  NetEngine a i -> NetEngInput a -> NetEngine a i
netEngineStep ne (LocalMove f) =
  ne { neLocalMove = f (neLocalMove ne) }
netEngineStep ne IterTimer =
  case peerMoves of
    Nothing -> ne { neWaitingForPeers = True }
    Just move ->
      ne
      { neLocalMove = mempty
      , neQueue = 
          insert moveKey (neLocalMove ne) $
          foldr delete (neQueue ne) moveKeys
      , neGameIteration = iter + 1
      , neOutput = return move
      }
  where
    moveKey = (iter + neLatencyIters ne, nePeerId ne)
    iter = neGameIteration ne
    moveKeys = ((,) iter) <$> nePeers ne
    peerMoves = 
      mconcat <$>
      sequence ((`lookup` neQueue ne) <$> moveKeys)

netEngineCleanup :: NetEngine a i -> NetEngine a i
netEngineCleanup ne = ne { neOutput = Nothing }

netEngine ::
  (Monoid a, Ord i) =>
  Event (a -> a) -> i -> EffectfulFunc () () () -> (Event a, SideEffect)
netEngine localMoves peerId (setIterTimer, iterTimer) =
  (moves, effects)
  where
    effects = setIterTimer (((), ()) <$ moves `merge` ereturn mempty)
    moves = eMapMaybe neOutput ne
    ne = escanl (netEngineStep . netEngineCleanup) startEngine neInput
    neInput = foldl1 merge
      [ LocalMove <$> localMoves
      , IterTimer <$ iterTimer
      ]
    latencyIters = 10
    startEngine = NetEngine
      { neLocalMove = mempty
      , neQueue =
          fromList
          [((i, peerId), mempty) | i <- [0 .. latencyIters - 1]]
      , neGameIteration = 0
      , neLatencyIters = latencyIters
      , nePeerId = peerId
      , nePeers = [peerId]
      , neWaitingForPeers = False
      , neOutput = Nothing
      }

