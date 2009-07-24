import Chess
import ChessFont

import Control.Monad (forM, join, when, unless)
import Data.Foldable (any, foldl', forM_, toList)
import Data.List (genericTake)
import Data.Monoid
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

import Prelude hiding (any)

faceNormal :: (Floating a, Ord a) => [[a]] -> [a]
faceNormal points =
  normalizeVec [a1*b2-a2*b1, a2*b0-a0*b2, a0*b1-a1*b0]
  where
    offset = head points
    base = map (zipWith (-) offset) (tail points)
    [[a0, a1, a2], [b0, b1, b2]] = base

normalizeVec vec
  | all (== 0) vec = vec
  | otherwise = map (/ norm) vec
  where
    norm = sqrt . sum $ map (^ 2) vec

screen2board :: DrawPos -> BoardPos
screen2board (cx, cy) =
  (r cx, r cy)
  where
    r ca = round (4 * ca + 3.5)

board2screen :: BoardPos -> DrawPos
board2screen (bx, by) =
  (r bx, r by)
  where
    r ba = (fromIntegral ba - 3.5) / 4

tailRot :: [a] -> [a]
tailRot [] = []
tailRot (x : xs) = xs ++ [x]

outlineSegments :: [a] -> [(a, a)]
outlineSegments xs = zip xs (tailRot xs)

expandOutline :: GLfloat -> [DrawPos] -> [DrawPos]
expandOutline ammount outline =
  last t : init t
  where
    t = map intersection $ outlineSegments segments
    intersection
      (a@((xa0, ya0), (xa1, ya1))
      ,b@((xb0, yb0), (xb1, yb1)))
      | xa0 == xa1 =
        (xa0, yb0 + (xa0-xb0) * (yb1-yb0) / (xb1-xb0))
      | xb0 == xb1 = intersection (b, a)
      | otherwise =
        (x, yAt0 a + x * d a)
      where
        x = (yAt0 a - yAt0 b) / (d b - d a)
    d ((x0, y0), (x1, y1)) = (y1-y0)/(x1-x0)
    yAt0 s@((x0, y0), _) = y0 - x0 * d s
    segments = map expandSegment $ outlineSegments outline
    expandSegment ((ax, ay), (bx, by)) =
      ((ax + ammount * nx, ay + ammount * ny)
      ,(bx + ammount * nx, by + ammount * ny))
      where
        [nx, ny] = normalizeVec [ay - by, bx - ax]

type Selection = (BoardPos, Maybe BoardPos)

draw :: (Board, (Selection, DrawPos)) -> Image
draw (board, ((dragSrc, dragDst), cpos@(cx, cy))) =
  Image $ do
    cursor $= None
    lineSmooth $= Enabled
    polygonSmooth $= Enabled
    hint LineSmooth $= Nicest
    hint PolygonSmooth $= Nicest
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= Vertex4 0 0 (-1) 0
    cullFace $= Nothing
    drawBoard
    forM_ (boardPieces board) drawPiece
    when (srcFirst dragDst) $ drawCursor dragSrc
    forM_ dragDst drawCursor
    unless (srcFirst dragDst) $ drawCursor dragSrc
  where
    srcFirst Nothing = True
    srcFirst (Just dst) = cursorDist dragSrc < cursorDist dst
    cursorDist = cursorDist' . board2screen
    cursorDist' (x, y) = (cx-x)^2 + (cy-y)^2
    headingUp = normal $ Normal3 0 0 (-1 :: GLfloat)
    drawPiece piece = do
      let
        pix = piecePix (pieceType piece)
        (px, py) = piecePos piece
        (sx, sy) = board2screen (px, py)
        white :: Color4 GLfloat
        white = Color4 1 1 1 1
        black = Color4 0 0 0 1
        (bodyCol, outlineCol)
          | pieceSide piece == White = (white, black)
          | otherwise = (black, white)
        vert m (vx, vy) =
          vertex $ Vertex4
            (sx + m*vx)
            (sy + m*vy) 0 1
      headingUp
      materialDiffuse Front $= bodyCol
      forM (pixBody pix) $ \poly -> do
        let
          polyType
            | 3 == length poly = Triangles
            | otherwise = Quads
        renderPrimitive polyType . forM poly . vert $ pieceSize * 0.125
      materialDiffuse Front $= outlineCol
      renderPrimitive Quads .
        forM (pixOutline pix) $ \outline ->
        forM (outlineSegments 
          (zip outline (expandOutline (-0.06) outline))) $
          \((a, b), (d, c)) ->
          forM [a, b, c, d] . vert $ pieceSize * 0.125
    pieceSize = 0.9
    drawBoard =
      forM_ [0..7] $ \bx ->
      forM_ [0..7] $ \by -> do
        let
          col = 0.3 + 0.1 * fromIntegral ((bx + by) `mod` 2)
          r ba va = 0.125*((fromIntegral ba*2+va)-7)
        materialDiffuse Front $= Color4 col col col 1
        headingUp
        renderPrimitive Quads .
          forM square $ \(vx, vy) ->
            vertex $ Vertex4 (r bx vx) (r by vy) 0 1
    drawCursor boardPos = do
      cullFace $= Just Back
      drawCursor' boardPos
      cullFace $= Just Front
      drawCursor' boardPos
    drawCursor' boardPos =
      renderPrimitive Triangles .
      forM_ curPix $ \part ->
      forM_ (outlineSegments part) $
      \((ax, ay), (bx, by)) -> do
        let
          (rx, ry) = board2screen boardPos
          points =
            [[0.9*cx, 0.9*cy, 0.9]
            ,[rx + 0.125*ax, ry + 0.125*ay, 1]
            ,[rx + 0.125*bx, ry + 0.125*by, 1]
            ]
          norml = faceNormal points
          [nx, ny, nz]
            | last norml < 0 = norml
            | otherwise = map negate norml
        normal $ Normal3 nx ny nz
        materialDiffuse Front $=
          case pieceUnderCursor of
            Nothing -> Color4 1 1 0 0.5
            otherwise -> Color4 0 1 0 1
        forM_ (take 1 points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
        materialDiffuse Front $=
          case pieceUnderCursor of
            Nothing -> Color4 1 1 0 0
            otherwise -> Color4 0 1 0 0.5
        forM_ (tail points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
    pieceUnderCursor = pieceAt board dragSrc
    curPix =
      case pieceUnderCursor of
        Nothing -> [square]
        Just p -> map (map t) . pixOutline . piecePix $ pieceType p
      where
        t (x, y) = (pieceSize*x, pieceSize*y)
    square = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]

keyState :: UI -> Key -> Event KeyState
keyState ui key =
  mappend (ereturn Up) .
  fmap m $
  efilter f (glutKeyboardMouseEvent ui)
  where
    m (_, state, _, _) = state
    f (k, _, _, _) = k == key

delayEvent :: Integral i => i -> Event a -> Event a
delayEvent count =
  fmap last .
  edrop count .
  escanl step []
  where
    step xs x = x : genericTake count xs

isGoodMove :: Board -> BoardPos -> BoardPos -> Bool
isGoodMove board src dst =
  any(any ((== dst) . fst) . possibleMoves board) $ pieceAt board src

maybeMinimumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maybeMinimumOn f =
  foldl' maybeMin Nothing
  where
    maybeMin Nothing x = Just x
    maybeMin (Just x) y
      | f y < f x = Just y
      | otherwise = Just x

chooseMove :: Board -> BoardPos -> DrawPos -> Maybe (BoardPos, Board)
chooseMove board src (dx, dy) =
  join .
  fmap (maybeMinimumOn (dist . fst) . possibleMoves board) $
  pieceAt board src
  where
    dist pos =
      (px-dx) ^ 2 + (py-dy) ^ 2
      where
        (px, py) = board2screen pos

game :: UI -> Event Image
game ui =
  fmap draw .
  ezip' board $
  ezip' selection (mouseMotionEvent ui)
  where
    board = escanl doMove chessStart moves
    procDst brd src = join . fmap (chooseMove brd src)
    doMove brd (src, dst) =
      case procDst brd src dst of
        Nothing -> brd
        Just r -> snd r
    selection =
      fmap proc .
      ezip' board $
      fmap snd selectionRaw
      where
        proc (brd, (src, dst)) =
          (src, fmap fst (procDst brd src dst))
    selectionRaw =
      edrop 1 .
      escanl drag (Up, undefined) $
      ezip' (keyState ui (MouseButton LeftButton)) (mouseMotionEvent ui)
    drag (Down, (x, _)) (Down, c) =
      (Down, (x, Just c))
    drag _ (s, c) =
      (s, (spos, dst))
      where
        spos = screen2board c
        dst
          | s == Up = Nothing
          | otherwise = Just c
    moves =
      fmap (snd . fst) $
      efilter moveFilter $
      ezip' (delayEvent 1 selectionRaw) selectionRaw
    moveFilter ((Down, _), (Up, _)) = True
    moveFilter _ = False

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [With DisplayRGB
    ,Where DisplaySamples IsAtLeast 2
    ]
  run game
