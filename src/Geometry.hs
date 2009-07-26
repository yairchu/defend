module Geometry (
  expandPolygon, faceNormal, normalizeVector,
  polygonEdges, triangulatePolygon
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

type Line a = ((a, a), (a, a))

slope :: Fractional a => Line a -> Maybe a
slope ((x0, y0), (x1, y1))
  | x0 == x1 = Nothing
  | otherwise = Just $ (y1-y0) / (x1-x0)

lineIntersection :: Fractional a => Line a -> Line a -> (a, a)
lineIntersection a@((xa0, _), _) b@((xb0, yb0), _) =
  case (slope a, slope b) of
    (Nothing, Just db) -> (xa0, yb0 + (xa0-xb0) * db)
    (_, Nothing) -> lineIntersection b a
    (Just da, Just db) ->
      let
        x = (yAt0 a da - yAt0 b db) / (db - da)
        yAt0 ((x0, y0), _) d = y0 - x0 * d
      in
        (x, yAt0 a da + x * da)

expandPolygon :: Floating a => a -> [(a, a)] -> [(a, a)]
expandPolygon ammount outline =
  last t : init t
  where
    t = map (uncurry lineIntersection) (polygonEdges segments)
    segments = map expandSegment (polygonEdges outline)
    expandSegment ((ax, ay), (bx, by)) =
      ((ax + ammount * nx, ay + ammount * ny)
      ,(bx + ammount * nx, by + ammount * ny))
      where
        [nx, ny] = normalizeVector [ay - by, bx - ax]

twicePolygonArea :: Num a => [(a, a)] -> a
twicePolygonArea =
  sum . map f . polygonEdges
  where
    f ((x0, y0), (x1, y1)) = x1*y0-x0*y1

isFrontPolygon :: (Ord a, Num a) => [(a, a)] -> Bool
isFrontPolygon = (>= 0) . twicePolygonArea

pointInPolygon :: (Ord a, Fractional a) => (a, a) -> [(a, a)] -> Bool
pointInPolygon (px, py) polygon =
  length (filter crosses (polygonEdges polygon)) `mod` 2 == 1
  where
    horizLine = ((0, py), (1, py))
    crosses e@((_, ey0), (_, ey1)) =
      ix > px && iy > min ey0 ey1 && iy < max ey0 ey1
      where
        (ix, iy) = lineIntersection horizLine e

triangulatePolygon :: (Ord a, Floating a) => [(a, a)] -> [[(a, a)]]
triangulatePolygon points
  | length points < 3 = []
  | not (isFrontPolygon points) = []
  | not (isFrontPolygon abc) ||
    any (`pointInPolygon` abc) rest =
    triangulatePolygon (tailRot points)
  | otherwise = abc : triangulatePolygon ([a, c] ++ rest)
  where
    (abc@[a, _, c], rest) = splitAt 3 points

