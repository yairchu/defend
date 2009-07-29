module Font (
  DefendFont, DrawPos, Pix(..),
  loadFont
  ) where

import Geometry (triangulatePolygon)

import Data.Map (Map)
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

