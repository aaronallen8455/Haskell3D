module Primitives where

import Lib
import Control.Monad (guard, replicateM)

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
    rotate r = rotateVect (Coord 0 r 0) (Coord 1 0 0)

-- | Constructs a sphere given a radius,
-- radial subdivisions, and vertical subdivisions.
sphere :: GU -> Int -> Int -> Maybe (Mesh Point)
sphere radius radialSubs verticalSubs
  | radius <= 0 = Nothing
  | radialSubs < 3 = Nothing
  | verticalSubs < 1 = Nothing
  | otherwise = do
    circles <- mapM (flip circlePoints radialSubs) radii
    let translated = zipWith (translatePoints (Coord 0 1 0)) heights circles
        indexed = zip [1..] $ concat translated
        edges = map makeEdge indexed
    return . meshFromEdges $ bottom : top : edges
  where
    vsrs = verticalSubs * radialSubs
    top = (Coord 0 (2 * radius) 0, vsrs + 1, take radialSubs [vsrs,vsrs-1..])
    bottom = (Coord 0 0 0, 0, [1..radialSubs])
    angle = 2 * pi / fromIntegral (verticalSubs * 2 + 2)
    radii = map ((*radius) . sin) $ take verticalSubs [angle, angle * 2..] :: [Radian]
    heights = map ((+radius) . (*radius) . negate . cos) $ take verticalSubs [angle, angle * 2..] :: [GU]
    -- make edges for an inner circle point
    makeEdge :: (Int, Point) -> (Point, Int, [Int])
    makeEdge (i, p) = (p, i, [above, below, left, right]) where
      above = min (vsrs + 1) $ i + radialSubs
      below = max 0 $ i - radialSubs
      mo = mod i radialSubs
      left | mo == 1 = i - 1 + radialSubs
           | otherwise = i - 1
      right | mo == 0 = i + 1 - radialSubs
            | otherwise = i + 1

-- | Construct a torus with given inner radius, outer radius, radial subdivisions,
-- and circle subdivisions.
torus :: GU -> GU -> Int -> Int -> Maybe (Mesh Point)
torus innerRad outerRad radialSubs circleSubs
  | any id (map (<0) [innerRad, outerRad]) = Nothing
  | any id (map (<3) [radialSubs, circleSubs]) = Nothing
  | otherwise = do
    edges <- map makeEdge . zip [0..] . concat 
           . zipWith ($) rotate . map (translate . flipUp)
           <$> replicateM radialSubs (circlePoints ((outerRad - innerRad) / 2) circleSubs)
    return $ meshFromEdges edges
  where
    rotations = [0, 2 * pi / fromIntegral radialSubs..] :: [Radian]
    rotate = map (\r -> rotatePoints mempty (Coord 0 r 0)) rotations
    circleCenter = innerRad + (outerRad - innerRad) / 2
    translate = translatePoints (Coord 1 0 0) circleCenter
    flipUp = rotatePoints mempty (Coord (pi/2) 0 0)
    numPoints = radialSubs * circleSubs
    makeEdge :: (Int, Point) -> (Point, Int, [Int])
    makeEdge (i, p) = (p, i, edges) where
      up = mod (i + circleSubs) numPoints
      down = mod (i - circleSubs) numPoints
      (q, r) = quotRem i circleSubs
      left = circleSubs * q + mod (r - 1) circleSubs
      right = circleSubs * q + mod (r + 1) circleSubs
      edges = [up, down, left, right]


--linePoints

--plane :: GU -> GU -> Int -> Int -> Maybe (Mesh Point)
--plane width depth wDiv lDiv
--  | width <= 0 = Nothing
--  | depth <= 0 = Nothing
--  | wDiv < 0 = Nothing
--  | lDiv < 0 = Nothing
--  | otherwise =
