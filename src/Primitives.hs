module Primitives where

import Lib
import Control.Monad (guard)

-- | create a circle with given radius and subdivision
circle :: GU -> Int -> Maybe (Mesh Point)
circle radius sub
  | radius <= 0 = Nothing
  | sub <= 2 = Nothing
  | otherwise = do
    pts <- circlePoints radius sub
    let edges = zip3 pts [0..] ls
    return . meshFromEdges $ edges
  where
    angle = 2 * pi / fromIntegral sub
    ls :: [[Int]]
    ls = zipWith (\a b -> [a, b]) (sub - 1 : [0..]) . reverse $ 0 : [sub-1, sub-2..1]

-- | create a circle of points
circlePoints :: GU -> Int -> Maybe [Point]
circlePoints radius sub
  | radius <= 0 = Nothing
  | sub <= 2 = Nothing
  | otherwise = Just . map (fmap (*radius) . rotate) $ take sub [0, angle..]
  where
    angle = 2 * pi / fromIntegral sub
    rotate :: Radian -> Point
    rotate r = rotateVect (Coord 1 0 0) (Coord 0 r 0)

-- | Constructs a sphere given a radius,
-- radial subdivisions, and vertical subdivisions.
sphere :: GU -> Int -> Int -> Maybe (Mesh Point)
sphere radius radialSubs verticalSubs
  | radius <= 0 = Nothing
  | radialSubs < 3 = Nothing
  | verticalSubs < 1 = Nothing
  | otherwise = do
    circles <- mapM (flip circlePoints radialSubs) radii
    let translated = zipWith (\c -> translatePoints c (Coord 0 1 0)) circles heights
        indexed = zip [1..] $ concat translated
        edges = map makeEdge indexed
    return . meshFromEdges $ bottom : top : edges
  where
    vsrs = verticalSubs * radialSubs
    top = (Coord 0 (2 * radius) 0, vsrs + 1, take radialSubs [vsrs,vsrs-1..])
    bottom = (Coord 0 0 0, 0, [1..radialSubs])
    angle = 2 * pi / fromIntegral (verticalSubs * 2 + 2)
    radii = map ((*radius) . sin) $ take verticalSubs [angle, angle * 2..] :: [Radian]
    step = 2 * radius / fromIntegral verticalSubs
    heights = take verticalSubs [step, step * 2..] :: [GU]
    -- make edges for an inner circle point
    makeEdge (i, p) = (p, i, [above, below, left, right]) where
      above = min (vsrs + 1) $ i + radialSubs
      below = max 0 $ i - radialSubs
      mo = mod i radialSubs
      left | mo == 1 = i - 1 + radialSubs
           | otherwise = i - 1
      right | mo == 0 = i + 1 - radialSubs
            | otherwise = i + 1
    
