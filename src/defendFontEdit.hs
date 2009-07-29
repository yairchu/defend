import Geometry
import UI

import Control.Monad
import Data.Map (Map, findWithDefault, insert)
import Data.Monoid
import FRP.Peakachu
import FRP.Peakachu.Backend.File
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT

gridRadius :: Int
gridRadius = 4

type DrawPos = (GLfloat, GLfloat)

findWithDef :: (Monoid a, Ord k) => k -> Map k a -> a
findWithDef = findWithDefault mempty

adjustWithDef :: (Monoid a, Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjustWithDef func key src =
  insert key (func (findWithDef key src)) src

draw :: (Map String [[DrawPos]], (String, DrawPos)) -> Image
draw (font, (text, cpos@(cx, cy))) =
  Image $ do
    color $ Color4 0.1 0.3 0.1 (1 :: GLfloat)
    renderPrimitive Triangles .
      forM (addPoint polygons cpos) $ \poly ->
      forM ((
        filter ((== 3) . length) .
        map (expandPolygon (-0.01)) .
        triangulatePolygon
        ) poly) .
      mapM $ \(x, y) ->
      vertex $ Vertex2 x y
    forM gridLines $ \x ->
      forM gridLines $ \y ->
      drawPoint x y 0.03 (Color4 0.5 0.5 0.5 1)
    forM (concat polygons) $ \(x, y) ->
      drawPoint x y 0.07 (Color4 0.3 1 0.2 1)
    drawPoint cx cy 0.05 (Color4 1 0.2 0.2 1)
    currentRasterPosition $= Vertex4 (-1) (-1) 0 (1 :: GLfloat)
    renderString Helvetica18 text
    return ()
  where
    polygons = findWithDef text font
    drawPoint :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat -> IO ()
    drawPoint x y s col =
      renderPrimitive Quads $ do
        color col
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

eZipFstTimes :: Event a -> Event b -> Event (a, b)
eZipFstTimes primary other =
  fmap m .
  efilter f .
  eWithPrev .
  (`ezip` other) .
  escanl step Nothing $ primary
  where
    step Nothing x = Just (False, x)
    step (Just (b, _)) y = Just (not b, y)
    f ((Just (x, _), _), (Just (y, _), _)) = x /= y
    f ((Nothing, _), (Just _, _)) = True
    f _ = False
    m (_, (bx, y)) =
      (x, y)
      where
        Just (_, x) = bx

atPress :: Key -> Modifiers -> Event a -> UI -> Event a
atPress key modi event =
  fmap snd .
  (`eZipFstTimes` event) .
  efilter f .
  glutKeyboardMouseEvent
  where
    f (k, Down, m, _) = k == key && m == modi
    f _ = False

noMods :: Modifiers
noMods = Modifiers Up Up Up

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

addPoint :: Eq a => [[a]] -> a -> [[a]]
addPoint [] x = [[x]]
addPoint ([] : ps) x = [x] : ps
addPoint (ys : ps) x
  | x `elem` ys = [] : ys : ps
  | otherwise = snoc x ys : ps

typedText :: UI -> Event Char
typedText =
  fmap m .
  efilter f .
  glutKeyboardMouseEvent
  where
    f (Char _, Down, Modifiers Up Up Up, _) = True
    f _ = False
    m (Char c, _, _, _) = c
    m _ = '#' -- should never get here

game :: UI -> (Event Image, SideEffect)
game = do
  mouse <- fmap (fmap toGrid) mouseMotionEvent
  chars <- typedText
  let
    text = escanl tstep [] chars
    tstep "" '\DEL' = ""
    tstep xs '\DEL' = init xs
    tstep "" '\b' = ""
    tstep xs '\b' = init xs
    tstep xs x = snoc x xs
    textNMouse = ezip text mouse
    clicks but =
      fmap (fmap ((,) but)) $ atPress (MouseButton but) noMods textNMouse
    filename = fmap (++ ".font") text
    altMod = Modifiers Up Up Down -- ctrl doesn't seem to work
  lClicks <- clicks LeftButton
  rClicks <- clicks RightButton
  load <-
    fmap read . readFileE .
    atPress (Char 'l') altMod filename
  let
    font =
      escanl step mempty .
      mappend (fmap Left load) .
      fmap Right .
      mappend lClicks $ rClicks
    step cur (Right (but, (key, point))) =
      adjustWithDef (func but) key cur
      where
        func LeftButton = (`addPoint` point)
        func _ =
          ([] :) . filter (not . null) .
          filter (notElem point)
    step _ (Left x) = x
    image =
      fmap draw .
      ezip font $ textNMouse
  save <-
    writeFileE .
    atPress (Char 's') altMod
    (ezip filename (fmap show font))
  return (image, save)

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [With DisplayRGB
    ,Where DisplaySamples IsAtLeast 2
    ]
  run game

