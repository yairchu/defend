{-# LANGUAGE TemplateHaskell #-}

import Paths_DefendTheKing (getDataFileName)
import Chess
import Draw
import Font
import GameLogic (Move(..), PartialData(..))
import Intro
import NetEngine
import NetMatching
import Networking

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Control.Monad ((>=>), guard, join)
import Data.ADT.Getters
import Data.List (foldl')
import Data.Map (Map, findWithDefault, insert)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Data.Time.Clock
import FRP.Peakachu
import FRP.Peakachu.Program
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Backend.GLUT.Getters
import FRP.Peakachu.Backend.StdIO
import FRP.Peakachu.Backend.Time
import Graphics.UI.GLUT hiding (Program, Exit)
import Network.Socket (SockAddr)
import System.Random (randomRIO)

import Prelude hiding ((.), id)

data MyTimers
  = TimerMatching
  | TimerGameIter
  | TimerTransmit
  deriving Show
$(mkADTGetters ''MyTimers)

data MoveLimitType
  = SinglePieceLimit BoardPos
  | GlobalMoveLimit
  deriving (Eq, Ord, Show)

data MyNode
  = IGlut UTCTime (GlutToProgram MyTimers)
  | IUdp (UdpToProg ())
  | IHttp (Maybe String)
  | OGlut (ProgramToGlut MyTimers)
  | OUdp (ProgToUdp ())
  | OHttp String
  | OPrint String
  | Exit
  | ABoard Board
  | ASelection Move
  | AQueueMove Move
  | AMoves [Move]
  | AResetBoard
  | ASide (Maybe PieceSide)
  | AMatching [SockAddr]
  | AMoveLimits (Map MoveLimitType Integer)
  | AGameIteration Integer
$(mkADTGetters ''MyNode)

maybeMinimumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maybeMinimumOn f =
  foldl' maybeMin Nothing
  where
    maybeMin Nothing x = Just x
    maybeMin (Just x) y
      | f y < f x = Just y
      | otherwise = Just x

distance :: DrawPos -> DrawPos -> GLfloat
distance (xA, yA) (xB, yB) =
  join (*) (xA-xB) + join (*) (yA-yB)

chooseMove :: Board -> BoardPos -> DrawPos -> Maybe (BoardPos, Board)
chooseMove board src drawPos =
  join $
  maybeMinimumOn (distance drawPos . board2screen . fst) . possibleMoves board <$>
  pieceAt board src

withPrev :: MergeProgram a (a, a)
withPrev =
  mapMaybeC (uncurry (liftA2 (,)))
  . MergeProg (scanlP step (Nothing, Nothing))
  where
    step (_, x) y = (x, Just y)

prevP :: MergeProgram a a
prevP = arrC fst . withPrev

singleValueP :: MergeProgram a ()
singleValueP = MergeProg . runAppendProg . return $ ()

keyState :: Key -> MergeProgram (GlutToProgram a) KeyState
keyState key =
  (lstPs . Just) Up (gKeyboardMouseEvent >=> f)
  where
    f (k, s, _, _) = do
      guard $ k == key
      return s

addP :: (Category cat, Monoid (cat a a)) => cat a a -> cat a a
addP = mappend id

atP :: FilterCategory cat => (a -> Maybe b) -> cat a b
atP = mapMaybeC

game :: Integer -> DefendFont -> Program MyNode MyNode
game myPeerId font =
  (takeWhileP (isNothing . gExit) .)
  . runMergeProg
  $ mconcat
  [ id
  , OGlut . DrawImage . mappend glStyle
    <$> (mappend
      <$> (draw font
        <$ atP gGlut
        <*> lstP gABoard
        <*> lstP gASelection
        <*> mouseMotion
        <*> lstP gASide
        <*> lstP gAGameIteration
        )
      <*> MergeProg (intro font) . arrC fst . atP gIGlut
      )
  , OUdp (CreateUdpListenSocket stunServer ()) <$ singleValueP
  , Exit <$ atP (gGlut >=> gKeyboardMouseEvent >=> quitButton)
  , OPrint . (++ "\n") . ("Limits: " ++) . show <$> atP gAMoveLimits
  ]
  -- loopback because board affects moves and vice versa
  . inMergeProgram1 (loopbackP lb) (
    addP neteng
    . addP calculateLimits
    . addP calculateMoves
    . addP calculateSelection
    -- calculate board state
    . addP
      ( ABoard <$> MergeProg (scanlP doMoves chessStart)
        . mconcat
        [ Just <$> atP gAMoves
        , Nothing <$ atP gAResetBoard
        ]
      )
  )
  . mconcat
  [ id
  , ASide Nothing <$ singleValueP
  -- contact http server
  , matching
  ]
  where
    lb =
      runMergeProg $ mconcat
      [ AMoves <$> atP gAMoves
      , AResetBoard <$ atP gAResetBoard
      , ASide <$> atP gASide
      , AMoveLimits <$> atP gAMoveLimits
      , AGameIteration <$> atP gAGameIteration
      ]
    quitButton (Char 'q', _, _, _) = Just ()
    quitButton _ = Nothing
    setTimerTransmit = OGlut $ SetTimer 25 TimerTransmit
    neteng =
      mconcat
      [ setTimerTransmit <$
        mconcat
        [ atP (gGlut >=> gTimerEvent >=> gTimerTransmit)
        , singleValueP
        ]
      , mconcat
        [ AMoves <$> atP gNEOMove
        , OGlut (SetTimer 50 TimerGameIter) <$ atP gNEOSetIterTimer
        , OUdp . ($ ()) . uncurry SendTo <$> atP gNEOPacket
        , ASide . Just . pickSide <$> atP gNEOPeerConnected
        , AResetBoard <$ atP gNEOPeerConnected
        , AGameIteration <$> atP gNEOGameIteration
        ]
        . netEngine myPeerId
        . mconcat
        [ NEIMatching <$> atP gAMatching
        , NEIMove . return <$> atP gAQueueMove
        , NEIIterTimer     <$ atP (gGlut >=> gTimerEvent >=> gTimerGameIter)
        , NEITransmitTimer <$ atP (gGlut >=> gTimerEvent >=> gTimerTransmit)
        , (\(a, b, _) -> NEIPacket a b) <$> atP (gIUdp >=> gRecvFrom)
        ]
      ]
    pickSide peerId
      | myPeerId < peerId = Black
      | otherwise = White
    matching =
      mconcat
      [ OHttp . fst <$> atP gMOHttp
      , OGlut (SetTimer 1000 TimerMatching) <$ atP gMOSetRetryTimer
      , OPrint . ("Matching:" ++) . (++ "\n") . show <$> atP gMatchingResult
      , AMatching . fst <$> atP gMatchingResult
      ]
      . MergeProg netMatching
      . mconcat
      [ arrC (`MIHttp` ()) . atP gIHttp
      , MITimerEvent () <$ atP (gGlut >=> gTimerEvent >=> gTimerMatching)
      , uncurry DoMatching <$> atP (gIUdp >=> gUdpSocketAddresses)
      ]
    mouseMotion = (lstPs . Just) (0, 0) (gGlut >=> gMouseMotionEvent)
    gGlut = (fmap . fmap) snd gIGlut
    calculateMoves =
      AQueueMove
      <$ (atP gUp <* atP gDown . prevP)
        . keyState (MouseButton LeftButton) . lstP gGlut
      <*> prevP . lstP gASelection
    calculateSelection =
      (rid .) $ aSelection
      <$> lstP gABoard
      <*> arrC snd . MergeProg (scanlP drag (Up, Move (0, 0) 0)) .
        ((,)
        <$> keyState (MouseButton LeftButton) . lstP gGlut
        <*> (calcMoveIter
          <$> rid . (selectionSrc
            <$> lstP gABoard
            <*> lstP gASide
            <*> mouseMotion
            )
          <*> lstP gAMoveLimits
          )
        )
      <*> mouseMotion
    calculateLimits =
      AMoveLimits <$>
      MergeProg (scanlP updateLimits mempty)
      . mconcat
      [ Just <$> ((,) <$> atP gAQueueMove <*> lstP gAGameIteration)
      , Nothing <$ atP (gAResetBoard)
      ]
    globalMoveLimit = 10
    pieceMoveLimit = 25
    updateLimits prev (Just (move, iter)) =
      insert GlobalMoveLimit (iter + globalMoveLimit)
      . insert (SinglePieceLimit (moveDst move)) (iter + pieceMoveLimit)
      $ prev
    updateLimits _ Nothing = mempty
    aSelection board move pos =
      ASelection . move . fst <$>
      chooseMove board (moveSrc (getPartial move)) pos
    calcMoveIter move limits =
      move
      . max (f GlobalMoveLimit)
      . f . SinglePieceLimit
      . moveSrc . getPartial $ move
      where
        f k = findWithDefault 0 k limits
    doMoves board (Just moves) = foldl doMove board moves
    doMoves _ Nothing = chessStart
    doMove board move =
      fromMaybe board
      $ pieceAt board (moveSrc move)
      >>= lookup (moveDst move) . possibleMoves board
    selectionSrc board side pos =
      fmap Move
      . maybeMinimumOn (distance pos . board2screen)
      . fmap piecePos
      . filter ((&&)
        <$> (/= Just False) . (<$> side) . (==) . pieceSide
        <*> canMove board)
      . boardPieces $ board
    canMove board = not . null . possibleMoves board
    drag (Down, pos) (Down, _) = (Down, pos)
    drag _ x = x

-- more options at http://www.voip-info.org/wiki/view/STUN
stunServer :: String
stunServer = "stun.ekiga.net"

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [ With DisplayRGB
    , Where DisplaySamples IsAtLeast 2
    ]
  font <- loadFont <$> (readFile =<< getDataFileName "data/defend.font")
  peerId <- randomRIO (0, 2^(128::Int))
  let
    backend =
      mconcat
      [ uncurry IGlut <$> getTimeB . glut . atP gOGlut
      , IHttp . fst <$> httpGetB . arrC (flip (,) ()) . atP gOHttp
      , IUdp <$> udpB . atP gOUdp
      , atP (const Nothing) . stdoutB . atP gOPrint
      ]
  runProgram backend (game peerId font)

