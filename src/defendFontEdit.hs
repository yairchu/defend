import Geometry
import UI

import Control.Monad
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

gridRadius :: Int
gridRadius = 4

type DrawPos = (GLfloat, GLfloat)

draw :: ([DrawPos], DrawPos) -> Image
draw (points, cpos@(cx, cy)) =
  Image $ do
    color $ Color4 0.1 0.3 0.1 (1 :: GLfloat)
    renderPrimitive Triangles .
      forM (triangulatePolygon (cpos : points)) .
      mapM $ \(x, y) ->
      vertex $ Vertex2 x y
    forM gridLines $ \x ->
      forM gridLines $ \y ->
      drawPoint x y 0.03 (Color4 0.5 0.5 0.5 1)
    forM points $ \(x, y) ->
      drawPoint x y 0.07 (Color4 0.3 1 0.2 1)
    drawPoint cx cy 0.05 (Color4 1 0.2 0.2 1)
    return ()
  where
    drawPoint :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat -> IO ()
    drawPoint x y s col =
      renderPrimitive Quads $ do
        color $ col
        vertex $ Vertex2 (x-s) y
        vertex $ Vertex2 x (y-s)
        vertex $ Vertex2 (x+s) y
        vertex $ Vertex2 x (y+s)
    gridLines :: [GLfloat]
    gridLines = map ((/ fromIntegral gridRadius) . fromIntegral) [-gridRadius..gridRadius]

toGrid :: DrawPos -> DrawPos
toGrid (x, y) =
  (t x, t y)
  where
    t =
      (/ fromIntegral gridRadius) .
      fromIntegral .
      (round :: GLfloat -> Int) .
      (* fromIntegral gridRadius)

atClick :: Key -> Event a -> UI -> Event a
atClick key event =
  fmap (fst . snd) .
  efilter isClick .
  eWithPrev .
  fmap (ezip' event) (keyState key)
  where
    isClick ((_, Up), (_, Down)) = True
    isClick _ = False

game :: UI -> Event Image
game = do
  mouse <- fmap (fmap toGrid) mouseMotionEvent
  clicks <- atClick (MouseButton LeftButton) mouse
  let
    outline = escanl step [] clicks
    step xs x = xs ++ [x]
  return . fmap draw $ ezip' outline mouse

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [With DisplayRGB
    ,Where DisplaySamples IsAtLeast 2
    ]
  run game

