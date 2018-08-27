module Primitives where

import           Control.Monad (guard, replicateM)
import           Data.Maybe
import           Debug.Trace
import           Lib

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
    rotate r = rotateVect (coord 0 r 0) (coord 1 0 0)

-- | Constructs a sphere given a radius,
-- radial subdivisions, and vertical subdivisions.
sphere :: GU -> Int -> Int -> Maybe (Mesh Point)
sphere radius radialSubs verticalSubs
  | radius <= 0 = Nothing
  | radialSubs < 3 = Nothing
  | verticalSubs < 1 = Nothing
  | otherwise = do
    circles <- mapM (flip circlePoints radialSubs) radii
    let translated = zipWith (translatePoints (coord 0 1 0)) heights circles
        indexed = zip [1..] $ concat translated
        edges = map makeEdge indexed
    return . meshFromEdges $ bottom : top : edges
  where
    vsrs = verticalSubs * radialSubs
    top = (coord 0 (2 * radius) 0, vsrs + 1, take radialSubs [vsrs,vsrs-1..])
    bottom = (coord 0 0 0, 0, [1..radialSubs])
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
    rotate = map (\r -> rotatePoints (coord 0 0 0) (coord 0 r 0)) rotations
    circleCenter = innerRad + (outerRad - innerRad) / 2
    translate = translatePoints (coord 1 0 0) circleCenter
    flipUp = rotatePoints (coord 0 0 0) (coord (pi/2) 0 0)
    numPoints = radialSubs * circleSubs
    makeEdge :: (Int, Point) -> (Point, Int, [Int])
    makeEdge (i, p) = (p, i, edges) where
      up = mod (i + circleSubs) numPoints
      down = mod (i - circleSubs) numPoints
      (q, r) = quotRem i circleSubs
      left = circleSubs * q + mod (r - 1) circleSubs
      right = circleSubs * q + mod (r + 1) circleSubs
      edges = [up, down, left, right]


-- | Produce a plane of points
planePoints :: GU -> GU -> Int -> Int -> Maybe [Point]
planePoints width len wDiv lDiv
  | any (<= 0) [width, len] = Nothing
  | any (< 0) [wDiv, lDiv] = Nothing
  | otherwise = Just [coord x 0 z | x <- ld, z <- wd]
  where
    wd = [0, width / fromIntegral (wDiv+1).. width]
    ld = [0, len / fromIntegral (lDiv+1).. len]

-- | Produce a square of points
squarePoints :: GU -> GU -> Int -> Int -> Maybe [Point]
squarePoints width len wDiv lDiv = do
  points <- planePoints width len wDiv lDiv
  let (s, r) = splitAt rw points
      (m, e) = splitAt (rw * lDiv) r
  return $ s ++ outer m ++ e
 where
  rw = wDiv + 2
  outer [] = []
  outer ps = let (r, rest) = splitAt rw ps in head r : last r : outer rest

-- | Creates a plane
plane :: GU -> GU -> Int -> Int -> Maybe (Mesh Point)
plane width len wDiv lDiv = do
  points <- flip zip [0..] <$> planePoints width len wDiv lDiv
  let edges = map makeEdge points
  return $ meshFromEdges edges
 where
   makeEdge (pt, i) = (pt, i, es) where
     es = catMaybes [l, r, u, d]
     row = mod i (wDiv+2)
     l = if row == 0 then Nothing else Just $ i - 1
     r = if row == wDiv+1 then Nothing else Just $ i + 1
     col = div i (wDiv+2)
     u = if mod col (lDiv+2) == 0 then Nothing else Just $ i - (wDiv+2)
     d = if mod col (lDiv+2) == lDiv+1 then Nothing else Just $ i + wDiv + 2

-- | Creates a box
box :: GU -> GU -> GU -> Int -> Int -> Int -> Maybe (Mesh Point)
box width len height wDiv lDiv hDiv = do
  base <- planePoints width len wDiv lDiv
  side <- squarePoints width len wDiv lDiv
  let heightStep = height / fromIntegral (hDiv + 1)
      sides = concat $ translatePoints (coord 0 1 0) <$> take hDiv [heightStep, heightStep*2..] <*> [side]
      top = translatePoints (coord 0 1 0) height base
      points = flip zip [0..] $ base ++ sides ++ top
      edges = map makeEdge points
  return $ meshFromEdges edges
 where

  baseSize = (wDiv + 2) * (lDiv + 2)
  numPts = baseSize * 2 + sideSize
  sideSize = hullSize * hDiv
  hullSize = (wDiv + 2) * 2 + lDiv * 2

  makeEdge (pt, i) = (pt, i, es) where
    isBase = i < baseSize
    isTop = i >= numPts - baseSize

    sideOffset = i - baseSize - hullSize * (levelI - 1)

    levelI | isBase = 0
           | isTop = hDiv + 1
           | otherwise = ((i - baseSize) `div` hullSize) + 1

    colI | isBase = mod i (wDiv + 2)
         | isTop = mod (i - baseSize - sideSize) (wDiv + 2)
         | rowI == 0 = sideOffset
         | rowI == lDiv + 1 = sideOffset - wDiv - 2 - lDiv * 2
         | otherwise = mod (sideOffset - wDiv - 2) 2 * (wDiv + 1)

    rowI | isBase = div i (wDiv + 2)
         | isTop = div (i - baseSize - sideSize) (wDiv + 2)
         | sideOffset < wDiv + 2 = 0
         | sideOffset >= hullSize - wDiv - 2 = lDiv + 1
         | otherwise = div (sideOffset - wDiv - 2) 2 + 1

    es = catMaybes [l, r, f, b, u, d]

    l | colI == 0 = Nothing
      | isTop || isBase = Just $ i - 1
      | rowI == 0 || rowI == lDiv + 1 = Just $ i - 1
      | otherwise = Nothing

    r | colI == wDiv + 1 = Nothing
      | isTop || isBase = Just $ i + 1
      | rowI == 0 || rowI == lDiv + 1 = Just $ i + 1
      | otherwise = Nothing

    f | rowI == lDiv + 1 = Nothing
      | isTop || isBase = Just $ i + wDiv + 2
      | colI /= 0 && colI /= wDiv + 1 = Nothing
      | rowI == lDiv && colI == wDiv + 1 = Just $ i + wDiv + 2
      | rowI /= 0 = Just $ i + 2
      | colI == 0 = Just $ i + wDiv + 2
      | otherwise = Just $ i + 2

    b | rowI == 0 = Nothing
      | isTop || isBase = Just $ i - wDiv - 2
      | colI /= 0 && colI /= wDiv + 1 = Nothing
      | rowI == 1 && colI == 0 = Just $ i - wDiv - 2
      | rowI /= lDiv + 1 = Just $ i - 2
      | colI == 0 = Just $ i - 2
      | otherwise = Just $ i - wDiv - 2

    u | isTop = Nothing
      | not $ any id [rowI == 0, rowI == lDiv + 1, colI == 0, colI == wDiv + 1] = Nothing
      | levelI == hDiv = Just $ numPts - baseSize + rowI * (wDiv + 2) + colI
      | not isBase = Just $ i + hullSize
      | rowI == 0 = Just $ baseSize + i
      | rowI == lDiv + 1 = Just $ baseSize + wDiv + 2 + 2 * lDiv + colI
      | colI == 0 = Just $ baseSize + wDiv + 2 + 2 * (rowI - 1)
      | otherwise = Just $ baseSize + wDiv + 2 + 2 * (rowI - 1) + 1

    d | isBase = Nothing
      | not $ any id [rowI == 0, rowI == lDiv + 1, colI == 0, colI == wDiv + 1] = Nothing
      | levelI == 1 = Just $ rowI * (wDiv + 2) + colI
      | not isTop = Just $ i - hullSize
      | rowI == 0 = Just $ numPts - baseSize - hullSize + colI
      | rowI == lDiv + 1 = Just $ i - baseSize
      | colI == 0 = Just $ numPts - baseSize - hullSize + wDiv + 2 + 2 * (rowI - 1)
      | otherwise = Just $ numPts - baseSize - hullSize + wDiv + 2 + 2 * (rowI - 1) + 1

dodecahedron :: GU -> Maybe (Mesh Point)
dodecahedron size
  | size <= 0 = Nothing
  | otherwise = Just $ meshFromEdges edges where
    sizeR = [-size, size]
    gr = (sqrt 5 - 1) / 2
    grp = [negate gr - 1, gr + 1]
    grs = [-1 + gr^2, 1 - gr^2]
    cube = [ coord x y z | x <- sizeR, y <- sizeR, z <- sizeR]
    cross = [ coord 0 y z | y <- grp, z <- grs ] ++
            [ coord x y 0 | x <- grp, y <- grs ] ++
            [ coord x 0 z | x <- grs, z <- grp ]
    pts = flip zip [0..] $ cube ++ cross
    -- the first 2 and last 2 of each cross group are connected
    makeEdge (pt, i) = (pt, i, filter (/= i) [0..length pts - 1])
    edges = map makeEdge pts