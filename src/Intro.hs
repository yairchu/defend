module Intro (intro) where

import Font
import Geometry

import Control.Applicative
import Control.Category
import Control.FilterCategory
import Control.Monad (forM_)
import Data.Map ((!))
import Data.Time.Clock
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Program
import Graphics.UI.GLUT hiding (Program)

import Prelude hiding ((.), id)

relativeTime :: Program UTCTime NominalDiffTime
relativeTime =
  uncurry diffUTCTime <$> rid . scanlP step Nothing
  where
    step Nothing x = Just (x, x)
    step (Just (_, start)) x = Just (x, start)

renderText :: DefendFont -> String -> [[DrawPos]]
renderText font text =
  concat . zipWith doLine lns $ zip rowCenters rowHeights
  where
    lns = lines text
    rowHeights = map ((2 /) . fromIntegral . length) lns
    top = sum rowHeights / 2
    rowTops = scanl (-) top rowHeights
    rowCenters = zipWith (flip (-) . (/ 2)) rowHeights rowTops
    doLine line (mid, size) =
      concat $ zipWith doLetter [(0 :: Int) ..] line
      where
        doLetter _ ' ' = []
        doLetter off letter =
          map (map (trans size (size * fromIntegral (2 * off + 1 - length line) / 2, mid))) . pixBody $ font ! [letter]
    trans s (dx, dy) (sx, sy) = (dx+s*sx/2, dy+s*sy/2)

intro :: DefendFont -> Program UTCTime Image
intro font =
  frame <$> relativeTime
  where
    frame t =
      Image $ do
        lighting $= Disabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        color $ Color4 0 0 0 (max 0 (4-f*0.3))
        renderPrimitive Quads .
          forM_ [(-1, -1), (-1, 1), (1, 1), (1, -1)] $ \(x, y) ->
            vertex $ Vertex4 x y (0 :: GLfloat) 1
        blendFunc $= (SrcAlpha, One)
        color $ Color4 1 0.5 0.25 (1 - abs (0.5+f*0.1-1))
        renderPrimitive Triangles .
          forM_ (expandPolygon e =<< renderText font "defend\nthe king\nfrom forces\nof different") $ \(x, y) ->
            vertex $ Vertex4 x y 0 z
      where
        f :: GLfloat
        f = 5 * realToFrac t
        z' = 2-f/5
        z = 1+(3*z'^(3::Int)+z')/4
        e' = f/90-0.1
        e = (e'*e'*e'*100*3+e')/3.5

