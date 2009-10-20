import Geometry

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Control.Monad
import Data.Map (Map, findWithDefault, insert)
import Data.Monoid
import FRP.Peakachu
import FRP.Peakachu.Program
import FRP.Peakachu.Backend.File
import FRP.Peakachu.Backend.GLUT
import Graphics.UI.GLUT hiding (Program)

import Prelude hiding ((.), id)

gridRadius :: Int
gridRadius = 4

type DrawPos = (GLfloat, GLfloat)

findWithDef :: (Monoid a, Ord k) => k -> Map k a -> a
findWithDef = findWithDefault mempty

adjustWithDef :: (Monoid a, Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjustWithDef func key src =
  insert key (func (findWithDef key src)) src

toGrid :: DrawPos -> DrawPos
toGrid (x, y) =
  (t x, t y)
  where
    t =
      (/ fromIntegral gridRadius) .
      fromIntegral .
      (round :: GLfloat -> Int) .
      (* fromIntegral gridRadius)

snoc :: a -> [a] -> [a]
snoc x = (++ [x])

addPoint :: Eq a => [[a]] -> a -> [[a]]
addPoint [] x = [[x]]
addPoint ([] : ps) x = [x] : ps
addPoint (ys : ps) x
  | x `elem` ys = [] : ys : ps
  | otherwise = snoc x ys : ps

data MyIn = Glut (GlutToProgram ()) | FileI (FileToProgram ())
data MyOut = GlutO (ProgramToGlut ()) | FileO (ProgramToFile ())

draw :: Map String [[DrawPos]] -> String -> DrawPos -> Image
draw font text cpos@(cx, cy) =
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
    gridLines = map ((/ fromIntegral gridRadius) . fromIntegral) [-gridRadius..gridRadius]

data MyMid
  = AText String
  | APos DrawPos
  | AClick MouseButton
  | AFont (Map String [[DrawPos]])
  | ADoLoad | ADoSave

-- ridArr
ra :: (FilterCategory cat, Functor (cat a)) => (a -> Maybe b) -> cat a b
ra = mapMaybeC

gameProc :: Program MyIn MyOut
gameProc =
  mconcat
  [ GlutO . DrawImage <$>
    (draw <$> ra aFont <*> ra aText <*> ra aPos)
  , FileO <$>
    (rid . (doLoad <$> id <*> ra aText))
    `mappend`
    (rid . (doSave <$> id <*> ra aText <*> ra aFont))
  ]
  . mconcat
  [ id
  , AFont <$> 
    scanlS fontStep mempty .
    ((,,) <$> id <*> ra aText <*> ra aPos)
  ]
  . mconcat
  [ AText <$> scanlS textStep [] . ra typedText
  , APos <$> toGrid <$> ra mouseMotion
  , AClick <$> ra clicksFunc
  , ADoLoad <$ ra (clicka (Char 'l') (Modifiers Up Up Down))
  , ADoSave <$ ra (clicka (Char 's') (Modifiers Up Up Down))
  , AFont <$> read <$> ra fileData
  ]
  where
    typedText (Glut (KeyboardMouseEvent
      (Char c) Down (Modifiers Up Up Up) _)) = Just c
    typedText _ = Nothing
    mouseMotion (Glut (MouseMotionEvent x y)) = Just (x, y)
    mouseMotion _ = Nothing
    fileData (FileI (FileData x ())) = Just x
    fileData _ = Nothing
    doLoad ADoLoad x =
      Just $ ReadFile (x ++ ".font") ()
    doLoad _ _ = Nothing
    doSave ADoSave fn fnt =
      Just $ WriteFile (fn ++ ".font") (show fnt) ()
    doSave _ _ _ = Nothing
    aText (AText x) = Just x
    aText _ = Nothing
    aPos (APos x) = Just x
    aPos _ = Nothing
    aFont (AFont x) = Just x
    aFont _ = Nothing
    textStep "" '\DEL' = ""
    textStep xs '\DEL' = init xs
    textStep "" '\b' = ""
    textStep xs '\b' = init xs
    textStep xs x = snoc x xs
    clicka key mods
      (Glut (KeyboardMouseEvent k Down m _))
      | k == key && m == mods = Just ()
      | otherwise = Nothing
    clicka _ _ _ = Nothing
    clicksFunc
      (Glut
      (KeyboardMouseEvent (MouseButton b) Down
      (Modifiers Up Up Up) _)) = Just b
    clicksFunc _ = Nothing
    fontStep prev (AClick LeftButton, text, pos) =
      adjustWithDef (`addPoint` pos) text prev
    fontStep prev (AClick _, text, pos) =
      adjustWithDef
      (([] :) . filter (not . null) .
      filter (notElem pos))
      text prev
    fontStep _ (AFont x, _, _) = x
    fontStep prev _ = prev

main :: IO ()
main = do
  initialWindowSize $= Size 600 600
  initialDisplayCapabilities $=
    [With DisplayRGB
    ,Where DisplaySamples IsAtLeast 2
    ]
  let
    backend =
      mconcat
      [ Glut <$> glut . ra fDraw
      , FileI <$> fileB . ra fFile
      ]
    fDraw (GlutO x) = Just x
    fDraw _ = Nothing
    fFile (FileO x) = Just x
    fFile _ = Nothing
  runProgram backend gameProc

