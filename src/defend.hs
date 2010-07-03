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
import Control.Monad ((>=>), forM_, guard, join)
import Data.ADT.Getters
import Data.Function (fix)
import Data.List (foldl')
import Data.Map (Map, findWithDefault, insert)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Data.Time.Clock
import Data.Traversable (sequenceA)
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
  | AText String
  | ABoard Board
  | ASelection Move
  | AQueueMove Move
  | AMoves [Move]
  | AResetBoard
  | ASide (Maybe PieceSide)
  | AMatching [SockAddr]
  | AMoveLimits (Map MoveLimitType Integer)
  | AGameIteration Integer
  | ALoopback MyNode
  | ADrawTimes UTCTime
  | AReadyForGame
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

keyState :: Key -> Program (GlutToProgram a) KeyState
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

genericCycle :: Monoid a => a -> a
genericCycle = fix . mappend

gGlut :: MyNode -> Maybe (GlutToProgram MyTimers)
gGlut = (fmap . fmap) snd gIGlut

matching :: Program MyNode MyNode
matching =
  mconcat
  [ OHttp . fst <$> atP gMOHttp
  , OGlut (SetTimer 1000 TimerMatching) <$ atP gMOSetRetryTimer
  , OPrint . ("Matching:" ++) . (++ "\n") . show <$> atP gMatchingResult
  , AMatching . fst <$> atP gMatchingResult
  ]
  . netMatching
  . mconcat
  [ arrC (`MIHttp` ()) . atP gIHttp
  , MITimerEvent () <$ atP (gGlut >=> gTimerEvent >=> gTimerMatching)
  , uncurry DoMatching <$> atP (gIUdp >=> gUdpSocketAddresses)
  ]

neteng :: Integer -> Program MyNode MyNode
neteng myPeerId =
  mconcat
  [ OGlut (SetTimer 25 TimerTransmit) <$
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
    , prepMoveToNe <$> atP gAQueueMove
    , NEIIterTimer     <$ atP (gGlut >=> gTimerEvent >=> gTimerGameIter)
    , NEITransmitTimer <$ atP (gGlut >=> gTimerEvent >=> gTimerTransmit)
    , (\(a, b, _) -> NEIPacket a b) <$> atP (gIUdp >=> gRecvFrom)
    ]
  ]
  where
    prepMoveToNe move = NEIMove (moveIter move) [move]
    pickSide peerId
      | myPeerId < peerId = Black
      | otherwise = White

drawText :: DefendFont -> String -> Image
drawText font text =
  Image $ do
    lighting $= Disabled
    color $ Color4 0.25 0 0.5 (0.5 :: GLfloat)
    renderPrimitive Triangles
      . (forM_ . join) (renderText font text)
      $ \(x, y) ->
      vertex $ Vertex4 (x/2) (y/2) 0 1

gNothing :: Maybe a -> Maybe ()
gNothing Nothing = Just ()
gNothing _ = Nothing

game :: Integer -> DefendFont -> Program MyNode MyNode
game myPeerId font =
  takeWhileP (isNothing . gExit)
  . mconcat
  [ id
  , OGlut . DrawImage . mconcat
    <$ atP gADrawTimes
    <*> sequenceA
    [ pure glStyle
    , draw font
        <$> lstP gABoard
        <*> lstP gASelection
        <*> mouseMotion
        <*> lstP gASide
        <*> lstP gAGameIteration
    , drawText font <$> lstP gAText
    , intro font . atP gADrawTimes
    ]
  , Exit <$ atP (gGlut >=> gKeyboardMouseEvent >=> quitButton)
  , OPrint "Got Udp Addr\n" <$ atP (gIUdp >=> gUdpSocketAddresses)
  ]
  -- loopback because board affects moves and vice versa
  . loopbackP (
    lb
    . addP (neteng myPeerId)
    . addP calculateLimits
    . addP calculateMoves
    . addP calculateSelection
    . addP calculateBoard
  )
  . addP emptyP
  . addP (
    withAppendProgram2
    mappend
    ( atP (const Nothing)
    . takeWhileP (isNothing . gAReadyForGame)
    )
    ( mconcat
      [ matching
      , OUdp (CreateUdpListenSocket stunServer ()) <$ singleValueP
      ]
    )
  )
  . mconcat
  [ id
  , drawTimes
  , ASide Nothing <$ singleValueP
  , mconcat
    [ AText <$> atP id
    , AReadyForGame <$ atP gNothing
    , AText "" <$ atP gNothing
    ]
    . arrC head
    . takeWhileP (not . null)
    . scanlP (flip (const tail)) instructions
    . atP (gGlut >=> gKeyboardMouseEvent >=> space)
  ]
  where
    instructions =
      map Just
      [ "welcome.\nget ready to\ndefend\nthe king.\npress space."
      , "drag and drop\nsome pieces.\nhit spacebar\nto resume."
      , "there are no turns.\nwhen the cursor\nis green\n"
        ++ "you can move.\nspace for more."
      , "when you\ndefend\nthe king\nyou will not\n"
        ++ "see the whole\nbattlefield.\nspace to see\nmore instructions."
      , "you will only\nsee the squares\nin reach of\n"
        ++ "your army.\nspace for\nnext message.."
      , "press space again\nto battle\nagainst a real\nking\n"
        ++ "like you.\nexcept evil."
      ] ++ [Nothing]
    globalMoveLimit = 20
    pieceMoveLimit = 50
    drawTimes =
      ADrawTimes <$> atP snd
      . scanlP drawTimeStep (Nothing, Nothing)
      . arrC fst . atP gIGlut
    drawTimeStep (Nothing, _) now = (Just now, Just now)
    drawTimeStep (Just prev, _) now
      | diffUTCTime now prev > 0.03 = (Just now, Just now)
      | otherwise = (Just prev, Nothing)
    resetOnResetBoard prog =
      runAppendProg . genericCycle $
      AppendProg prog . (AppendProg . takeWhileP) (isNothing . (gALoopback >=> gAResetBoard))
    calculateBoard =
      resetOnResetBoard $
      ABoard <$> scanlP (foldl doMove) chessStart
      . atP (gALoopback >=> gAMoves)
    lb =
      mconcat
      [ Left . ALoopback <$> filterC (isNothing . gALoopback)
      , Right <$> id
      ]
    space (Char ' ', Down, _, _) = Just ()
    space _ = Nothing
    quitButton (Char 'q', _, _, _) = Just ()
    quitButton _ = Nothing
    mouseMotion = (lstPs . Just) (0, 0) (gGlut >=> gMouseMotionEvent)
    calculateMoves =
      AQueueMove
      <$ (atP gUp <* atP gDown . delayP (1 :: Int))
        . keyState (MouseButton LeftButton) . lstP gGlut
      <*> delayP (1 :: Int) . lstP gASelection
    rid = genericFlattenC
    calculateSelection =
      (rid .) $ aSelection
      <$> lstP gABoard
      <*> arrC snd . scanlP drag (Up, Move (0, 0) White 0) .
        ((,)
        <$> keyState (MouseButton LeftButton) . lstP gGlut
        <*> (calcMoveIter
          <$> rid . (selectionSrc
            <$> lstP gABoard
            <*> lstP (gALoopback >=> gASide)
            <*> mouseMotion
            )
          <*> lstP (gALoopback >=> gAMoveLimits)
          )
        )
      <*> mouseMotion
    calculateLimits =
      resetOnResetBoard $
      AMoveLimits <$> scanlP updateLimits mempty
      . ( (,)
          <$> atP gAQueueMove
          <*> lstP (gALoopback >=> gAGameIteration)
        )
    updateLimits prev (move, iter) =
      insert GlobalMoveLimit (iter + globalMoveLimit)
      . insert (SinglePieceLimit (moveDst move)) (iter + pieceMoveLimit)
      $ prev
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
    doMove board move =
      fromMaybe board $ do
        piece <- pieceAt board . moveSrc $ move
        guard $ pieceSide piece == movePlayer move
        lookup (moveDst move) . possibleMoves board $ piece
    selectionSrc board side pos =
      fmap (Move <$> piecePos <*> pieceSide)
      . maybeMinimumOn (distance pos . board2screen . piecePos)
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

