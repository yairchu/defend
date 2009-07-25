module Geometry (
  expandPolygon, faceNormal, normalizeVector, polygonEdges
  ) where

faceNormal :: (Floating a, Ord a) => [[a]] -> [a]
faceNormal points =
  normalizeVector [a1*b2-a2*b1, a2*b0-a0*b2, a0*b1-a1*b0]
  where
    offset = head points
    base = map (zipWith (-) offset) (tail points)
    [[a0, a1, a2], [b0, b1, b2]] = base

normalizeVector :: Floating a => [a] -> [a]
normalizeVector vec
  | all (== 0) vec = vec
  | otherwise = map (/ norm) vec
  where
    norm = sqrt . sum $ map (^ (2 :: Int)) vec

polygonEdges :: [a] -> [(a, a)]
polygonEdges xs = zip xs (tailRot xs)

tailRot :: [a] -> [a]
tailRot [] = []
tailRot (x : xs) = xs ++ [x]

expandPolygon :: Floating a => a -> [(a, a)] -> [(a, a)]
expandPolygon ammount outline =
  last t : init t
  where
    t = map intersection $ polygonEdges segments
    intersection
      (a@((xa0, _), (xa1, _))
      ,b@((xb0, yb0), (xb1, yb1)))
      | xa0 == xa1 =
        (xa0, yb0 + (xa0-xb0) * (yb1-yb0) / (xb1-xb0))
      | xb0 == xb1 = intersection (b, a)
      | otherwise =
        (x, yAt0 a + x * d a)
      where
        x = (yAt0 a - yAt0 b) / (d b - d a)
    d ((x0, y0), (x1, y1)) = (y1-y0)/(x1-x0)
    yAt0 s@((x0, y0), _) = y0 - x0 * d s
    segments = map expandSegment $ polygonEdges outline
    expandSegment ((ax, ay), (bx, by)) =
      ((ax + ammount * nx, ay + ammount * ny)
      ,(bx + ammount * nx, by + ammount * ny))
      where
        [nx, ny] = normalizeVector [ay - by, bx - ax]

