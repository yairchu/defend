module Font
  ( DefendFont, DrawPos, Pix(..)
  , loadFont, renderText
  ) where

import Geometry (triangulatePolygon)

import Data.Map (Map, (!))
import Graphics.UI.GLUT (GLfloat)

type DrawPos = (GLfloat, GLfloat)

data Pix = Pix {
  pixOutline :: [[DrawPos]],
  -- | pixBody can (and is) be calculated from pixOutline
  -- it is in Pix for memoing this calculation.
  pixBody :: [[DrawPos]]
}

type DefendFont = Map String Pix

loadFont :: String -> DefendFont
loadFont =
  fmap loadChar . read
  where
    loadChar polygons =
      Pix {
        pixOutline = polygons,
        pixBody = triangulatePolygon =<< polygons
      }

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

