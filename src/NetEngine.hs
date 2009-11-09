{-# LANGUAGE TemplateHaskell #-}

module NetEngine
  ( NetEngineOutput(..), NetEngineInput(..)
  , netEngine
  , gNEOMove, gNEOPacket, gNEOSetIterTimer
  ) where

import Networking

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Codec.Compression.Zlib (decompress, compress)
import Control.Applicative ((<$), (<$>))
import Control.Monad (guard, forM_)
import Data.ADT.Getters
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Function (on)
import Data.Map (Map, delete, fromList, insert, lookup, toList)
import Data.Monoid (Monoid(..))
import FRP.Peakachu.Program
import Network.Socket (SockAddr)
import Prelude hiding ((.), id, lookup)

data NetEngineState moveType idType = NetEngineState
  { neLocalMove :: moveType
  , neQueue :: Map (Integer, idType) moveType
  , nePeers :: [idType]
  , nePeerAddrs :: [SockAddr]
  , neWaitingForPeers :: Bool
  , neGameIteration :: Integer
  , neOutputMove :: moveType
  , neLatencyIters :: Integer
  , neMyPeerId :: idType
  }

data NetEngineOutput moveType
  = NEOMove moveType
  | NEOPacket String SockAddr
  | NEOSetIterTimer
  deriving Show
$(mkADTGetters ''NetEngineOutput)

data NetEngineInput moveType
  = NEIMove moveType
  | NEIPacket String SockAddr
  | NEIMatching [SockAddr]
  | NEIIterTimer
  | NEITransmitTimer
$(mkADTGetters ''NetEngineInput)

data HelloType = LetsPlay | WereOn
  deriving (Read, Show, Eq)

data NetEngPacket m i
  = Moves (Map (Integer, i) m)
  | Hello i HelloType
  deriving (Read, Show)

atP :: FilterCategory cat => (a -> Maybe b) -> cat a b
atP = mapMaybeC

singleValueP :: b -> MergeProgram a b
singleValueP = MergeProg . runAppendProg . return

withPrev :: MergeProgram a (a, a)
withPrev =
  mapMaybeC (uncurry (liftA2 (,)))
  . MergeProg (scanlP step (Nothing, Nothing))
  where
    step (_, x) y = (x, Just y)

filterP :: FilterCategory cat => (a -> Bool) -> cat a a
filterP cond =
  mapMaybeC f
  where
    f x = do
      guard $ cond x :: Maybe ()
      return x

netEngine
  :: ( Monoid moveType, Ord peerIdType
     , Read moveType, Read peerIdType
     )
  => peerIdType
  -> MergeProgram (NetEngineInput moveType) (NetEngineOutput moveType)
netEngine myPeerId =
  mconcat
  [ singleValueP NEOSetIterTimer
  , mconcat
    [ mconcat
      [ NEOMove <$> arrC (neOutputMove . snd)
      , NEOSetIterTimer <$ id
      ]
      . filterP (uncurry (on (/=) neGameIteration))
      . withPrev
    ]
    . MergeProg (scanlP netEngineStep start)
--  , atP gNEIMatching
  ]
  where
    start = NetEngineState
      { neLocalMove = mempty
      , neQueue =
          fromList
          [ ((i, myPeerId), mempty)
          | i <- [0 .. latencyIters-1]
          ]
      , nePeers = [myPeerId]
      , nePeerAddrs = mempty
      , neWaitingForPeers = False
      , neGameIteration = 0
      , neOutputMove = mempty
      , neLatencyIters = latencyIters
      , neMyPeerId = myPeerId
      }
    latencyIters = 5

netEngineStep ::
  (Monoid a, Ord i, Read a, Read i) =>
  NetEngineState a i -> NetEngineInput a -> NetEngineState a i
netEngineStep state (NEIMove move) =
  state { neLocalMove = mappend move . neLocalMove $ state }
netEngineStep state NEIIterTimer = netEngineNextIter state
netEngineStep state (NEIMatching addrs) = state

netEngineNextIter ::
  (Monoid a, Ord i) => NetEngineState a i -> NetEngineState a i
netEngineNextIter ne =
  case peerMoves of
    Nothing -> ne { neWaitingForPeers = True }
    Just move ->
      ne
      { neLocalMove = mempty
      , neQueue =
          insert moveKey (neLocalMove ne) $
          foldr delete (neQueue ne) moveKeys
      , neGameIteration = iter + 1
      , neOutputMove = move
      , neWaitingForPeers = False
      }
  where
    moveKey = (iter + neLatencyIters ne, neMyPeerId ne)
    iter = neGameIteration ne
    moveKeys = (,) iter <$> nePeers ne
    peerMoves =
      mconcat <$>
      sequence ((`lookup` neQueue ne) <$> moveKeys)

{-

data NetEngineInput moveType idType = NetEngineInput
  { neiLocalMoveUpdates :: Event (moveType -> moveType)
  , neiPeerId :: idType
  , neiSocket :: PeakaSocket
  , neiNewPeerAddrs :: Event SockAddr
  , neiIterTimer :: EffectFunc () () ()
  , neiTransmitTimer :: EffectFunc () () ()
  }

data NetEngineOutput moveType idType = NetEngineOutput
  { neoMove :: Event moveType
  , neoSideEffect :: SideEffect
  , neoIsConnected :: Event Bool
  , neoPeers :: Event [idType]
  }

data NetEngEvent a
  = LocalMove (a -> a)
  | IterTimer
  | Recv (String, Int, SockAddr)

data NetEngPacket m i
  = Moves (NEQueue m i)
  | Hello i HelloType
  deriving (Read, Show)

data HelloType = LetsPlay | WereOn
  deriving (Read, Show, Eq)

netEngineStep ne (LocalMove f) =
  ne { neLocalMove = f (neLocalMove ne) }
netEngineStep ne (Recv (msg, _, peerAddr)) =
  netEngineProcPacket ne
  ((read . unpack . decompress . pack) msg) peerAddr

netEngineProcPacket ::
  (Monoid a, Ord i, Read a, Read i) =>
  NetEngine a i -> NetEngPacket a i -> SockAddr -> NetEngine a i
netEngineProcPacket ne (Moves upd) _
  | neWaitingForPeers ne = netEngineNextIter neUpd
  | otherwise = neUpd
  where
    neUpd = ne
      { neQueue =
          neQueue ne `mappend`
          (fromList . filter filtMove . toList) upd
      }
    filtMove ((iter, _), _) = iter >= neGameIteration ne
netEngineProcPacket ne (Hello peer hType) peerAddr
  | 2 == length (nePeers ne) = ne
  | otherwise = ne
    { nePeers = peer : nePeers ne
    , nePeerAddrs = [peerAddr]
    , neGameIteration = 0
    , neQueue = netEngineInitialQueue (nePeerId ne) (neLatencyIters ne)
    , neOutputPacket = do
        guard $ hType == LetsPlay
        return (Hello (nePeerId ne) WereOn, peerAddr)
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
  forall a i. (Monoid a, Ord i, Read a, Read i, Show a, Show i) =>
  NetEngineInput a i -> NetEngineOutput a i
netEngine nei =
  NetEngineOutput
  { neoMove = eMapMaybe neOutputMove ne
  , neoSideEffect = mconcat
    [ efRun (neiIterTimer nei) $ ((), ()) <$ moves `merge` ereturn mempty
    , efRun (neiTransmitTimer nei) $ ((), ()) <$ efOut (neiTransmitTimer nei) `merge` ereturn ((), ())
    , mkSideEffect (uncurry sendPacket) (eMapMaybe neOutputPacket ne)
    , mkSideEffect (sendPacket letsPlayPacket) (neiNewPeerAddrs nei)
    , mkSideEffect transmit (eZipByFst (efOut (neiTransmitTimer nei)) ne)
    ]
  , neoIsConnected = (> 1) . length . nePeers <$> ne
  , neoPeers = nePeers <$> ne
  }
  where
    transmit (_, n) =
      forM_ (nePeerAddrs n) (sendPacket (Moves (neQueue n)))
    letsPlayPacket :: NetEngPacket a i
    letsPlayPacket = Hello (neiPeerId nei) LetsPlay
    peerId = neiPeerId nei
    localMoves = neiLocalMoveUpdates nei
    sendPacket packet addr =
      () <$
      sendTo ((psSocket . neiSocket) nei)
      ((unpack . compress . pack . show) packet) addr
    moves = eMapMaybe neOutputMove ne
    ne = escanl (netEngineStep . netEngineCleanup) startEngine neInput
    neInput = foldl1 merge
      [ LocalMove <$> localMoves
      , IterTimer <$ efOut (neiIterTimer nei)
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
-}
