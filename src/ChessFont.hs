module ChessFont (DrawPos, PiecePix(..), piecePix) where

import Chess (PieceType(..))

import Graphics.UI.GLUT (GLfloat)

type DrawPos = (GLfloat, GLfloat)

data PiecePix = PiecePix {
  pixBody :: [[DrawPos]],
  pixOutline :: [[DrawPos]]
}

piecePix :: PieceType -> PiecePix
piecePix Pawn =
  PiecePix r r
  where
    r = [[(-s, -s), (-s, s), (s, s), (s, -s)]]
    s = 0.6
piecePix Rook = PiecePix {
  pixBody = [thing, othing, [(-s, -s), (-s, s), (s, s), (s, -s)]],
  pixOutline = [thing ++ othing]
  }
  where
    s = 0.5
    thing = [(-s, s), (-1, 1), (1, 1), (s, s)]
    othing = map r thing
    r (x, y) = (-x, -y)
piecePix Knight = PiecePix {
  pixBody = [[a, b, e] ,[b, c, d]],
  pixOutline = [outline]
  }
  where
    outline = [(-1, 0), (1, 1), (0.5, -1), (-1, -1), (0, 0)]
    [a, b, c, d, e] = outline
piecePix Bishop = PiecePix {
  pixBody = [[a, b, d], [b, c, d]],
  pixOutline = [outline]
  }
  where
    outline = [(-1, -1), (0, 1), (1, -1), (0, -0.5)]
    [a, b, c, d] = outline
piecePix King = PiecePix {
  pixBody = [leye, reye, [a, b, e], [c, d, e]],
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
    [a, b, c, d, e] = mouthline
piecePix Queen = PiecePix {
  pixBody = [[a, b, j], [d, e, f], [c, h, i], [c, g, h]],
  pixOutline = [outline]
  }
  where
    outline =
      [(-1, 1), (-0.25, 0.5), (0, 1), (0.25, 0.5), (1, 1)
      ,(0.5, 0), (1, -1), (0, -0.5), (-1, -1), (-0.5, 0)]
    [a, b, c, d, e, f, g, h, i, j] = outline


