module ChessFont (DrawPos, PiecePix(..), piecePix) where

import Chess (PieceType(..))
import Geometry (triangulatePolygon)

import Graphics.UI.GLUT (GLfloat)

type DrawPos = (GLfloat, GLfloat)

data PiecePix = PiecePix {
  pixOutline :: [[DrawPos]],
  -- | pixBody can (and is) be calculated from pixOutline
  -- it is in PiecePix for memoing this calculation.
  pixBody :: [[DrawPos]]
}

mkPix :: [[DrawPos]] -> PiecePix
mkPix outlines =
  PiecePix {
    pixOutline = outlines,
    pixBody = outlines >>= triangulatePolygon
  }

piecePix :: PieceType -> PiecePix
piecePix Pawn =
  mkPix [[(-s, -s), (-s, s), (s, s), (s, -s)]]
  where
    s = 0.6
piecePix Rook =
  mkPix [thing ++ othing]
  where
    s = 0.5
    thing = [(-s, s), (-1, 1), (1, 1), (s, s)]
    othing = map r thing
    r (x, y) = (-x, -y)
piecePix Knight =
  mkPix [[(-1, 0), (1, 1), (0.5, -1), (-1, -1), (0, 0)]]
piecePix Bishop =
  mkPix [[(-1, -1), (0, 1), (1, -1), (0, -0.5)]]
piecePix King =
  mkPix [reverse leye, reye, mouthline]
  where
    reye = [(0.25, 0.5), (0.5, 0.75), (0.75, 0.5), (0.5, 0.25)]
    leye = map m reye
    m (x, y) = (-x, y)
    mouthline =
      [(-0.75, -0.75), (-0.75, -0.25)
      ,(0, -0.5)
      ,(0.75, -0.25), (0.75, -0.75)]
piecePix Queen =
  mkPix [
    [(-1, 1), (-0.25, 0.5), (0, 1), (0.25, 0.5), (1, 1)
    ,(0.5, 0), (1, -1), (0, -0.5), (-1, -1), (-0.5, 0)]]

