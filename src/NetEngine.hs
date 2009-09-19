module NetEngine where

import Networking

import Control.Applicative ((<$), (<$>))
import Control.Monad (guard)
import Data.Map (Map, delete, fromList, insert, lookup, toList)
import Data.Monoid (Monoid(..))
import FRP.Peakachu (
  EffectfulFunc, Event, SideEffect,
  eMapMaybe, ereturn, escanl, merge)
import FRP.Peakachu.Backend.IO (mkSideEffect)
import Network.Socket (SockAddr, sendTo)
import Prelude hiding (lookup)

type NEQueue moveType idType = Map (Integer, idType) moveType

data NetEngine moveType idType = NetEngine
  { neLocalMove :: moveType
  , neQueue :: NEQueue moveType idType
  , neLatencyIters :: Integer
  , nePeerId :: idType
  , nePeers :: [idType]
  , nePeerAddrs :: [SockAddr]
  , neWaitingForPeers :: Bool
  , neGameIteration :: Integer
  , neOutputMove :: Maybe moveType
  , neOutputPacket :: Maybe (NetEngPacket moveType idType, SockAddr)
  }

data NetEngineInput moveType idType = NetEngineInput
  { neiLocalMoveUpdates :: Event (moveType -> moveType)
  , neiPeerId :: idType
  , neiIterTimer :: EffectfulFunc () () ()
  , neiSocket :: PeakaSocket
  }

data NetEngEvent a
  = LocalMove (a -> a)
  | IterTimer
  | Recv (String, Int, SockAddr)

data NetEngPacket m i
  = Moves (NEQueue m i)
  | Hello i HelloType
  deriving (Read, Show)

data HelloType = Let'sPlay | We'reOn
  deriving (Read, Show, Eq)

netEngineStep ::
  (Monoid a, Ord i, Read a, Read i) =>
  NetEngine a i -> NetEngEvent a -> NetEngine a i
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
      , neOutputMove = return move
      }
  where
    moveKey = (iter + neLatencyIters ne, nePeerId ne)
    iter = neGameIteration ne
    moveKeys = ((,) iter) <$> nePeers ne
    peerMoves = 
      mconcat <$>
      sequence ((`lookup` neQueue ne) <$> moveKeys)
netEngineStep ne (Recv (msg, _, peerAddr)) =
  netEngineProcPacket ne (read msg) peerAddr

netEngineProcPacket ::
  (Monoid a, Ord i, Read a, Read i) =>
  NetEngine a i -> NetEngPacket a i -> SockAddr -> NetEngine a i
netEngineProcPacket ne (Moves upd) _ =
  ne { neQueue = neQueue ne `mappend`
  (fromList . filter filtMove . toList) upd }
  where
    filtMove ((iter, _), _) = iter >= neGameIteration ne
netEngineProcPacket ne (Hello peer hType) peerAddr
  | 2 == length (nePeers ne) = ne
  | otherwise = ne
    { nePeers = peer : nePeers ne
    , nePeerAddrs = [peerAddr]
    , neGameIteration = 0
    , neQueue = netEngineInitialQueue (nePeerId ne) (neLatencyIters ne)
    , neOutputPacket = do
        guard $ hType == Let'sPlay
        return (Hello (nePeerId ne) We'reOn, peerAddr)
    }

netEngineCleanup :: NetEngine a i -> NetEngine a i
netEngineCleanup ne = ne
  { neOutputMove = Nothing
  , neOutputPacket = Nothing
  }

netEngineInitialQueue ::
  (Monoid a, Ord i) => i -> Integer -> NEQueue a i
netEngineInitialQueue peerId latencyIters =
  fromList [((i, peerId), mempty) | i <- [0 .. latencyIters - 1]]

netEngine ::
  (Monoid a, Ord i, Read a, Read i, Show a, Show i) =>
  NetEngineInput a i -> (Event a, SideEffect)
netEngine nei =
  (moves, effects)
  where
    (setIterTimer, iterTimer) = neiIterTimer nei
    peerId = neiPeerId nei
    localMoves = neiLocalMoveUpdates nei
    effects = mconcat
      [ setIterTimer (((), ()) <$ moves `merge` ereturn mempty)
      , mkSideEffect sendPacket . eMapMaybe neOutputPacket $ ne
      ]
    sendPacket (packet, addr) =
      () <$ sendTo ((psSocket . neiSocket) nei) (show packet) addr
    moves = eMapMaybe neOutputMove ne
    ne = escanl (netEngineStep . netEngineCleanup) startEngine neInput
    neInput = foldl1 merge
      [ LocalMove <$> localMoves
      , IterTimer <$ iterTimer
      , Recv <$> (psRecv . neiSocket) nei
      ]
    latencyIters = 10
    startEngine = NetEngine
      { neLocalMove = mempty
      , neQueue = netEngineInitialQueue peerId latencyIters
      , neGameIteration = 0
      , neLatencyIters = latencyIters
      , nePeerId = peerId
      , nePeers = [peerId]
      , nePeerAddrs = []
      , neWaitingForPeers = False
      , neOutputMove = Nothing
      , neOutputPacket = Nothing
      }

