{-# LANGUAGE TemplateHaskell #-}

module NetEngine
  ( NetEngineOutput(..), NetEngineInput(..)
  , netEngine
  , gNEOMove, gNEOPacket, gNEOSetIterTimer
  , gNEOPeerConnected, gNEOGameIteration
  ) where

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Codec.Compression.Zlib (decompress, compress)
import Control.Monad ((>=>))
import Data.ADT.Getters
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import Data.Function (on)
import Data.List (isPrefixOf, partition)
import Data.Map (Map, delete, fromList, insert, lookup, toList)
import Data.Monoid (Monoid(..))
import FRP.Peakachu.Program
import Network.Socket (SockAddr)
import Prelude hiding ((.), id, lookup)

data NetEngineState moveType idType = NetEngineState
  { neLocalQueue :: [(Integer, moveType)]
  , neQueue :: Map (Integer, idType) moveType
  , nePeers :: [idType]
  , nePeerAddrs :: [SockAddr]
  , neWaitingForPeers :: Bool
  , neGameIteration :: Integer
  , neOutputMove :: moveType
  , neLatencyIters :: Integer
  , neMyPeerId :: idType
  }

data NetEngineOutput moveType idType
  = NEOMove moveType
  | NEOPeerConnected idType
  | NEOPacket String SockAddr
  | NEOSetIterTimer
  | NEOGameIteration Integer
  deriving Show
$(mkADTGetters ''NetEngineOutput)

data NetEngineInput moveType
  = NEIMove Integer moveType
  | NEIPacket String SockAddr
  | NEIMatching [SockAddr]
  | NEIIterTimer
  | NEITransmitTimer
$(mkADTGetters ''NetEngineInput)

data NetEngineMid moveType idType
  = AInput (NetEngineInput moveType)
  | AState (NetEngineState moveType idType)
$(mkADTGetters ''NetEngineMid)

data HelloType = LetsPlay | WereOn
  deriving (Read, Show, Eq)

data NetEngPacket m i
  = Moves (Map (Integer, i) m)
  | Hello i HelloType
  deriving (Read, Show)

magic :: String
magic = "dtkffod!"

atP :: FilterCategory cat => (a -> Maybe b) -> cat a b
atP = mapMaybeC

outPacket
  :: (Show a, Show i)
  => NetEngPacket a i -> SockAddr -> NetEngineOutput a i
outPacket = NEOPacket . (magic ++) . withPack compress . show

withPrev :: MergeProgram a (a, a)
withPrev = (,) <$> delayP (1::Int) <*> id

atChgOf :: Eq b
  => (a -> b) -> MergeProgram a a
atChgOf onfunc =
  arrC snd . filterC (uncurry (on (/=) onfunc)) . withPrev

netEngine
  :: ( Monoid moveType, Ord peerIdType
     , Read moveType, Read peerIdType
     , Show moveType, Show peerIdType
     )
  => peerIdType
  -> MergeProgram
     (NetEngineInput moveType)
     (NetEngineOutput moveType peerIdType)
netEngine myPeerId =
  mconcat
  [ NEOSetIterTimer <$ singleValueP
  , mconcat
    [ mconcat
      [ NEOMove . neOutputMove <$> id
      , NEOSetIterTimer <$ id
      , NEOGameIteration . neGameIteration <$> id
      ] . atChgOf neGameIteration . atP gAState
    , mconcat
      [ outPacket (Hello myPeerId WereOn)
        <$> flattenC
        . arrC nePeerAddrs
      , arrC NEOPeerConnected
        . filterC (/= myPeerId)
        . flattenC
        . arrC nePeers
      ]
      . atChgOf nePeerAddrs
      . atP gAState
    , flattenC 
      . ( sendMoves
          <$> (lstP gAState <* atP (gAInput >=> gNEITransmitTimer))
        )
    ]
    . mconcat
    [ AState <$> scanlP netEngineStep (startState myPeerId)
    , arrC AInput
    ]
  , outPacket (Hello myPeerId LetsPlay) <$> flattenC . atP gNEIMatching
  -- for warnings
  , unwarn gNEIIterTimer
  , unwarn gNEIMove
  , unwarn gNEIPacket
  ]
  where
    unwarn x = undefined <$> filterC (const False) . atP x
    sendMoves state =
      outPacket (Moves (neQueue state)) <$> nePeerAddrs state

startState
  :: (Monoid moveType, Ord peerIdType)
  => peerIdType -> NetEngineState moveType peerIdType
startState myPeerId =
  NetEngineState
  { neLocalQueue = []
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
  where
    latencyIters = 5

netEngineStep
  :: (Monoid a, Ord i, Read a, Read i)
  => NetEngineState a i -> NetEngineInput a -> NetEngineState a i
netEngineStep state (NEIMove iter move) =
  state { neLocalQueue = (iter, move) : neLocalQueue state }
netEngineStep state NEIIterTimer = netEngineNextIter state
netEngineStep state (NEIPacket contents sender)
  | magic `isPrefixOf` contents =
    processPacket state sender
    . read . withPack decompress
    . drop (length magic) $ contents
  | otherwise = state
netEngineStep state _ = state

withPack :: (ByteString -> ByteString) -> String -> String
withPack = (unpack .) . (. pack)

processPacket
  :: (Monoid a, Ord i)
  => NetEngineState a i -> SockAddr -> NetEngPacket a i
  -> NetEngineState a i
processPacket state sender (Hello peerId _)
  | length (nePeers state) > 1 = state
  | otherwise =
    (startState myPeerId)
    { nePeers = [myPeerId, peerId]
    , nePeerAddrs = [sender]
    }
  where
    myPeerId = neMyPeerId state
processPacket state _ (Moves moves)
  | neWaitingForPeers state = netEngineNextIter updState
  | otherwise = updState
  where
    updState =
      state
      { neQueue =
          mappend (neQueue state) . fromList
          . filter ((>= neGameIteration state) . fst . fst)
          . toList $ moves
      }

netEngineNextIter ::
  (Monoid a, Ord i) => NetEngineState a i -> NetEngineState a i
netEngineNextIter ne =
  case peerMoves of
    Nothing -> ne { neWaitingForPeers = True }
    Just move ->
      ne
      { neLocalQueue = futureMoves
      , neQueue =
          insert moveKey (mconcat (snd <$> nextMoves)) $
          foldr delete (neQueue ne) delKeys
      , neGameIteration = iter + 1
      , neOutputMove = move
      , neWaitingForPeers = False
      }
  where
    (nextMoves, futureMoves) =
      partition ((<= nextMoveIter) . fst)
      . neLocalQueue $ ne
    nextMoveIter = iter + neLatencyIters ne
    moveKey = (nextMoveIter, neMyPeerId ne)
    iter = neGameIteration ne
    moveKeys = (,) iter <$> nePeers ne
    delKeys = (,) (iter - neLatencyIters ne) <$> nePeers ne
    peerMoves =
      mconcat <$>
      sequence ((`lookup` neQueue ne) <$> moveKeys)

