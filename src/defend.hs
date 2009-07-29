import Chess
import Font
import Paths_DefendTheKing (getDataFileName)
import Geometry
import UI

import Control.Monad (forM, join, liftM2, when, unless)
import Data.Foldable (foldl', forM_)
import Data.Char (toLower)
import Data.Map ((!))
import Data.Monoid
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

piecePix :: DefendFont -> PieceType -> Pix
piecePix font x = font ! map toLower (show x)

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

type Selection = (BoardPos, Maybe BoardPos)

draw :: DefendFont -> (Board, (Selection, DrawPos)) -> Image
draw font (board, ((dragSrc, dragDst), (cx, cy))) =
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
    cullFace $= Just Front
    drawBoard
    forM_ (boardPieces board) drawPiece
    when (srcFirst dragDst) $ drawCursor dragSrc
    forM_ dragDst drawCursor
    unless (srcFirst dragDst) $ drawCursor dragSrc
  where
    srcFirst Nothing = True
    srcFirst (Just dst) = cursorDist dragSrc < cursorDist dst
    cursorDist = cursorDist' . board2screen
    cursorDist' (x, y) = (cx-x)^(2::Int) + (cy-y)^(2::Int)
    headingUp = normal $ Normal3 0 0 (-1 :: GLfloat)
    drawPiece piece = do
      let
        pix = piecePix font (pieceType piece)
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
      renderPrimitive Triangles .
        forM (pixBody pix) .
        mapM $ vert (pieceSize * 0.125)
      materialDiffuse Front $= outlineCol
      renderPrimitive Quads .
        forM (pixOutline pix) $ \outline ->
        forM (polygonEdges
          (zip outline (expandPolygon (-0.06) outline))) $
          \((a, b), (c, d)) ->
          forM [c, d, b, a] . vert $ pieceSize * 0.125
    pieceSize = 0.9
    drawBoard =
      forM_ [0..7] $ \bx ->
      forM_ [0..7] $ \by -> do
        let
          col = 0.3 + 0.1 * fromIntegral ((bx + by) `mod` (2::Int))
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
      forM_ (polygonEdges part) $
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
            _ -> Color4 0 1 0 1
        forM_ (take 1 points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
        materialDiffuse Front $=
          case pieceUnderCursor of
            Nothing -> Color4 1 1 0 0
            _ -> Color4 0 1 0 0.5
        forM_ (tail points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
    pieceUnderCursor = pieceAt board dragSrc
    curPix =
      case pieceUnderCursor of
        Nothing -> [square]
        Just p -> map (map t) . pixOutline . piecePix font $ pieceType p
      where
        t (x, y) = (pieceSize*x, pieceSize*y)
    square = [((-1), (-1)), ((-1), 1), (1, 1), (1, (-1))]

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
      (px-dx)^(2::Int) + (py-dy)^(2::Int)
      where
        (px, py) = board2screen pos

game :: DefendFont -> UI -> (Event Image, SideEffect)
game font = do
  let
    drag (Down, (x, _)) (Down, c) =
      (Down, (x, Just c))
    drag _ (s, c) =
      (s, (spos, dst))
      where
        spos = screen2board c
        dst
          | s == Up = Nothing
          | otherwise = Just c
  selectionRaw <-
    edrop (1::Int) .
    escanl drag (Up, undefined) .
    liftM2 ezip (keyState (MouseButton LeftButton)) mouseMotionEvent
  let
    board = escanl doMove chessStart moves
    procDst brd src = join . fmap (chooseMove brd src)
    doMove brd (src, dst) =
      case procDst brd src dst of
        Nothing -> brd
        Just r -> snd r
    selection =
      fmap proc .
      ezip board $
      fmap snd selectionRaw
      where
        proc (brd, (src, dst)) =
          (src, fmap fst (procDst brd src dst))
    moves =
      fmap (snd . fst) .
      efilter moveFilter $
      eWithPrev selectionRaw
    moveFilter ((Down, _), (Up, _)) = True
    moveFilter _ = False
  image <- fmap (draw font) .
    ezip board .
    ezip selection .
    mouseMotionEvent
  return (image, mempty)

main :: IO ()
main = do
  font <- fmap loadFont . readFile =<< getDataFileName "data/defend.font"
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [With DisplayRGB
    ,Where DisplaySamples IsAtLeast 2
    ]
  run (game font)

