module ChessFont (DrawPos, PiecePix(..), piecePix, pixBody) where

import Chess (PieceType(..))
import Geometry (triangulatePolygon)

import Graphics.UI.GLUT (GLfloat)

type DrawPos = (GLfloat, GLfloat)

data PiecePix = PiecePix {
  pixOutline :: [[DrawPos]]
}

calcBody :: [[DrawPos]] -> [[DrawPos]]
calcBody = concatMap triangulatePolygon

pixBody :: PiecePix -> [[DrawPos]]
pixBody = calcBody . pixOutline

piecePix :: PieceType -> PiecePix
piecePix Pawn =
  PiecePix r
  where
    r = [[(-s, -s), (-s, s), (s, s), (s, -s)]]
    s = 0.6
piecePix Rook = PiecePix {
  pixOutline = [thing ++ othing]
  }
  where
    s = 0.5
    thing = [(-s, s), (-1, 1), (1, 1), (s, s)]
    othing = map r thing
    r (x, y) = (-x, -y)
piecePix Knight = PiecePix {
  pixOutline = [outline]
  }
  where
    outline = [(-1, 0), (1, 1), (0.5, -1), (-1, -1), (0, 0)]
piecePix Bishop = PiecePix {
  pixOutline = [outline]
  }
  where
    outline = [(-1, -1), (0, 1), (1, -1), (0, -0.5)]
piecePix King = PiecePix {
  pixOutline = [reverse leye, reye, mouthline]
  }
  where
    reye = [(0.25, 0.5), (0.5, 0.75), (0.75, 0.5), (0.5, 0.25)]
    leye = map m reye
    m (x, y) = (-x, y)
    mouthline =
      [(-0.75, -0.75), (-0.75, -0.25)
      ,(0, -0.5)
      ,(0.75, -0.25), (0.75, -0.75)]
piecePix Queen = PiecePix {
  pixOutline = [outline]
  }
  where
    outline =
      [(-1, 1), (-0.25, 0.5), (0, 1), (0.25, 0.5), (1, 1)
      ,(0.5, 0), (1, -1), (0, -0.5), (-1, -1), (-0.5, 0)]


