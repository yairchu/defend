import Control.Monad
import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

gridRadius :: Int
gridRadius = 4

type DrawPos = (GLfloat, GLfloat)

draw :: ([DrawPos], DrawPos) -> Image
draw (points, (cx, cy)) =
  Image $ do
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

--firstClickPos :: Key -> UI -> Event DrawPos
--firstClickPos key ui =
  --where
    --base = ezip' (keyState ui key) (mouseMotionEvent ui)

game :: UI -> Event Image
game ui =
  fmap draw .
  ezip' outlines .
  fmap toGrid . mouseMotionEvent $ ui
  where
    outlines = ereturn []

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [With DisplayRGB
    ,Where DisplaySamples IsAtLeast 2
    ]
  run game

