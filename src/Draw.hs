module Draw 
  ( draw, glStyle, board2screen
  ) where

import Chess
import Font
import GameLogic (Move(..), visibleSquares)
import Geometry

import Control.Applicative
import Control.Monad (forM, forM_, guard, unless, when)
import Data.Char (toLower)
import Data.Map ((!))
import FRP.Peakachu.Backend.GLUT (Image(..))
import Graphics.UI.GLUT

piecePix :: DefendFont -> PieceType -> Pix
piecePix font x = font ! map toLower (show x)

board2screen :: BoardPos -> DrawPos
board2screen (bx, by) =
  (r bx, r by)
  where
    r ba = (fromIntegral ba - 3.5) / 4

glStyle :: Image
glStyle = Image $ do
  cursor $= None
  lineSmooth $= Enabled
  polygonSmooth $= Enabled
  hint LineSmooth $= Nicest
  hint PolygonSmooth $= Nicest
  blend $= Enabled

draw
  :: DefendFont -> Board -> Move
  -> DrawPos -> Maybe PieceSide -> Integer -> Image
draw font board drag (cx, cy) me gameIter =
  Image $ do
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lighting $= Enabled
    light (Light 0) $= Enabled
    position (Light 0) $= Vertex4 0 0 (-1) 0
    cullFace $= Just Front
    drawBoard
    mapM_ drawPiece . filter ((`elem` vis) . piecePos) $ boardPieces board
    when srcFirst . drawCursor . moveSrc $ drag
    drawCursor . moveDst $ drag
    unless srcFirst . drawCursor . moveSrc $ drag
  where
    srcFirst = cursorDist (moveSrc drag) < cursorDist (moveDst drag)
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
        forM_ (pixBody pix) .
        mapM_ $ vert (pieceSize * 0.125)
      materialDiffuse Front $= outlineCol
      renderPrimitive Quads .
        forM (pixOutline pix) $ \outline ->
        forM (polygonEdges
          (zip outline (expandPolygon (-0.06) outline))) $
          \((a, b), (c, d)) ->
          forM [c, d, b, a] . vert $ pieceSize * 0.125
    pieceSize = 0.9
    vis =
      maybe allBoard (`visibleSquares` board) me
    allBoard = [(x, y) | x <- [0..7], y <- [0..7]]
    drawBoard =
      forM_ vis $ \(bx, by) -> do
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
            _ -> cursorColor 1
        forM_ (take 1 points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
        materialDiffuse Front $=
          case pieceUnderCursor of
            Nothing -> Color4 1 1 0 0
            _ -> cursorColor 0.5
        forM_ (tail points) $ \[px, py, pz] ->
          vertex $ Vertex4 px py 0 pz
    cursorColor
      | gameIter >= moveIter drag = Color4 0 1 0
      | otherwise = Color4 1 0 0
    pieceUnderCursor = do
      r <- pieceAt board . moveSrc $ drag
      guard . not . (== Just False) . (<$> me) . (==) . pieceSide $ r
      return r
    curPix =
      case pieceUnderCursor of
        Nothing -> [square]
        Just p -> map (map t) . pixOutline . piecePix font $ pieceType p
      where
        t (x, y) = (pieceSize*x, pieceSize*y)
    square = [(-1, -1), (-1, 1), (1, 1), (1, -1)]


