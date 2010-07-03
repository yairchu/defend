{-# LANGUAGE TemplateHaskell #-}

import Geometry

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Control.Monad
import Data.ADT.Getters
import Data.Map (Map, findWithDefault, insert)
import Data.Monoid
import FRP.Peakachu
import FRP.Peakachu.Program
import FRP.Peakachu.Backend.File
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Backend.GLUT.Getters
import Graphics.UI.GLUT hiding (Name, Program, get)

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

data MyOut
  = GlutO (ProgramToGlut ())
  | FileO (ProgramToFile ())

data MidLayer
  = AText String
  | APos DrawPos
  | AClick MouseButton
  | AFont (Map String [[DrawPos]])
  | ADoLoad | ADoSave

$(mkADTGetters ''MyIn)
$(mkADTGetters ''MyOut)
$(mkADTGetters ''MidLayer)

draw :: Map String [[DrawPos]] -> String -> DrawPos -> Image
draw font text cpos@(cx, cy) =
  Image $ do
    color $ Color4 0.1 0.3 0.1 (1 :: GLfloat)
    renderPrimitive Triangles .
      forM_ (addPoint polygons cpos) $ \poly ->
      forM_ ((
        filter ((== 3) . length) .
        map (expandPolygon (-0.01)) .
        triangulatePolygon
        ) poly) .
      mapM_ $ \(x, y) ->
      vertex $ Vertex2 x y
    forM_ gridLines $ \x ->
      forM_ gridLines $ \y ->
      drawPoint x y 0.03 (Color4 0.5 0.5 0.5 1)
    forM_ (concat polygons) $ \(x, y) ->
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

gameProc :: Program MyIn MyOut
gameProc =
  mconcat
  [ GlutO . DrawImage <$> (draw <$> lstP gAFont <*> lstP gAText <*> lstP gAPos)
  , FileO <$> mconcat
    [ doLoad <$ mapMaybeC gADoLoad <*> lstP gAText
    , doSave <$ mapMaybeC gADoSave <*> lstP gAText <*> lstP gAFont
    ]
  ]
  . mconcat
  [ id
  , AFont <$> 
    scanlP fontStep mempty .
    ( (,,)
      <$> mconcat
        [ Left <$> mapMaybeC gAClick
        , Right <$> mapMaybeC gAFont
        ]
      <*> lstP gAText <*> lstP gAPos
    )
  ]
  . mconcat
  [ mconcat
    [ AText <$> scanlP textStep [] . mapMaybeC typedText
    , ADoLoad <$ mapMaybeC (clicka (Char 'l') (Modifiers Up Up Down))
    , ADoSave <$ mapMaybeC (clicka (Char 's') (Modifiers Up Up Down))
    , AClick <$> mapMaybeC clicksFunc
    ] . mapMaybeC (gGlut >=> gKeyboardMouseEvent)
  , APos <$> toGrid <$> mapMaybeC (gGlut >=> gMouseMotionEvent)
  , AFont <$> read . fst <$> mapMaybeC (gFileI >=> gFileData)
  ]
  where
    typedText (c, s, m, _) = do
      guard $ m == Modifiers Up Up Up
      gDown s
      gChar c
    doLoad x = ReadFile (x ++ ".font") ()
    doSave fn fnt = WriteFile (fn ++ ".font") (show fnt) ()
    textStep "" '\DEL' = ""
    textStep xs '\DEL' = init xs
    textStep "" '\b' = ""
    textStep xs '\b' = init xs
    textStep xs x = snoc x xs
    clicka key mods (k, s, m, _) = do
      guard $ k == key && m == mods
      gDown s
    clicksFunc (key, state, _, _) = do
      gDown state
      gMouseButton key
    fontStep prev (Left LeftButton, text, pos) =
      adjustWithDef (`addPoint` pos) text prev
    fontStep prev (Left _, text, pos) =
      adjustWithDef
      (([] :) . filter (not . null) .
      filter (notElem pos))
      text prev
    fontStep _ (Right x, _, _) = x

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
      [ Glut <$> glut . mapMaybeC gGlutO
      , FileI <$> fileB . mapMaybeC gFileO
      ]
  runProgram backend gameProc

