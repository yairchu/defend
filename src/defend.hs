{-# LANGUAGE TemplateHaskell #-}

import Paths_DefendTheKing (getDataFileName)
import Chess
import Draw
import Font
import Intro
import NetEngine
import NetMatching
import Networking

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Control.Monad (guard, join)
import Data.ADT.Getters
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Time.Clock
import FRP.Peakachu
import FRP.Peakachu.Program
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Backend.GLUT.Getters
import FRP.Peakachu.Backend.Time
import Graphics.UI.GLUT hiding (Program)
import System.Random (randomRIO)

import Prelude hiding ((.), id)

data MyLayer
  = Glut (GlutToProgram ())
  | ABoard Board
  | ASelection BoardPos BoardPos
  | AMove BoardPos BoardPos
  | ASide (Maybe PieceSide)
  | AMousePos DrawPos
$(mkADTGetterCats ''MyLayer)

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

withPrev :: Program a (a, a)
withPrev =
  mapMaybeC (uncurry (liftA2 (,))) . scanlP step (Nothing, Nothing)
  where
    step (_, x) y = (x, Just y)

prevP :: Program a a
prevP = fst <$> withPrev

keyState :: Key -> Program (GlutToProgram a) KeyState
keyState key =
  mconcat
  [ singleValueP Up
  , mapMaybeC f . cKeyboardMouseEvent
  ]
  where
    f (k, s, _, _) = do
      guard $ k == key
      return s

game :: DefendFont -> Program (UTCTime, GlutToProgram ()) Image
game font =
  (draw
  <$> pure font
  <*> cABoard
  <*> cASelection
  <*> cAMousePos
  <*> cASide
  )
  . loopbackP (uncurry AMove <$> cAMove) (
    mconcat
    [ id
    , rid . (aMove
      <$> ((,) <$> id <*> prevP)
        . (const id <$> id <*> keyState (MouseButton LeftButton) . cGlut)
      <*> prevP . cASelection
      )
    ]
    . mconcat
    [ id
    , rid . (aSelection
      <$> cABoard
      <*> arr snd . scanlP drag (Up, (0, 0)) .
        ((,)
        <$> keyState (MouseButton LeftButton) . cGlut
        <*> rid . (selectionSrc
          <$> cABoard
          <*> cASide
          <*> cAMousePos
          )
        )
      <*> cAMousePos
      )
    ]
    . mconcat
    [ id
    , ABoard <$> scanlP doMove chessStart . cAMove
    , ASide <$> singleValueP (Just White)
    , AMousePos <$> mconcat
        [singleValueP (0, 0), cMouseMotionEvent . cGlut]
    ]
  )
  . arr (Glut . snd)
  where
    aMove (u, d) (src, dst) = do
      gUp u
      gDown d
      return $ AMove src dst
    aSelection board src pos =
      ASelection src . fst <$> chooseMove board src pos
    doMove board (src, dst) =
      fromMaybe board $
      pieceAt board src >>= lookup dst . possibleMoves board
    selectionSrc board side pos =
      maybeMinimumOn
      (distance pos . board2screen)
      . fmap piecePos
      . filter ((&&)
        <$> (/= Just False) . (<$> side) . (==) . pieceSide
        <*> canMove board)
      $ boardPieces board
    canMove board = not . null . possibleMoves board
    drag (Down, pos) (Down, _) = (Down, pos)
    drag _ x = x

{-
game :: DefEnv -> UI -> (Event Image, SideEffect)
game env ui =
  (image, effect)
  where
    drag (Down, (x, _)) (Down, c) =
      (Down, (x, Just c))
    drag _ (s, c) =
      (s, (spos, dst))
      where
        spos = screen2board c
        dst = do
          guard $ s == Down
          return c
    selectionRaw =
      edrop (1::Int) .
      escanl drag (Up, undefined) $
      (,) <$> keyState (MouseButton LeftButton) ui <*> mouseMotionEvent ui
    neo = netEngine NetEngineInput
      { neiLocalMoveUpdates = (:) <$> queuedMoves
      , neiPeerId = defClientId env
      , neiSocket = defSock env
      , neiNewPeerAddrs = matchingAddrs
      , neiIterTimer = setTimerTime 50 (defGameIterTimer env)
      , neiTransmitTimer = setTimerTime 20 (defTransmitTimer env)
      }
    (matchingAddrs, matchingEffects) =
      netMatching (defSock env) (defHttp env)
      (setTimerTime 1000 (defSrvRetryTimer env))
      (neoIsConnected neo)
    board = escanl doMove chessStart . eFlatten . neoMove $ neo
    procDst brd src = join . fmap (chooseMove brd src)
    doMove brd (src, dst) =
      case procDst brd src dst of
        Nothing -> brd
        Just r -> snd r
    selection =
      proc <$> board <*> selectionRaw
    proc brd (_, (src, dst)) =
      ( src
      , fst <$> (dst >>= chooseMove brd src))
    moveFilter ((Down, _), (Up, _)) = True
    moveFilter _ = False
    queuedMoves =
      snd . fst <$> efilter moveFilter (eWithPrev selectionRaw)
    mySide = calcSide <$> neoPeers neo
    calcSide peers
      | 1 == length peers = Nothing
      | defClientId env == minimum peers = Just Black
      | otherwise = Just White
    image =
      draw env <$> board <*> selection <*> mouseMotionEvent ui <*> mySide
    effect = mconcat
      [ neoSideEffect neo
      , matchingEffects
      ]
-}

-- more options at http://www.voip-info.org/wiki/view/STUN
stunServer :: String
stunServer = "stun.ekiga.net"

{-
initEnv :: IO DefEnv
initEnv =
  pure DefEnv
    <*> randomRIO (0, 2^(128::Int))
    <*> (fmap loadFont . readFile =<< getDataFileName "data/defend.font")
    <*> mkPeakaSocket stunServer
    <*> httpGet
    <*> setTimerEvent
    <*> setTimerEvent
    <*> setTimerEvent
-}

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [ With DisplayRGB
    , Where DisplaySamples IsAtLeast 2
    ]
  font <- loadFont <$> (readFile =<< getDataFileName "data/defend.font")
  let
    program =
      DrawImage <$> mappend glStyle <$>
      (mappend <$> game font <*> intro font . arr fst)
      --intro font . arr fst
  runProgram (getTimeB . glut) program
  --initEnv >>= run . prog

