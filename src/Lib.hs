{-# language RecordWildCards #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveGeneric #-}

module Lib where

import Control.Monad (guard, foldM)
import Data.Semigroup ((<>))
import Data.Maybe (catMaybes, isNothing, isJust)
import Data.Fixed (mod')
import Data.Foldable
import Data.Array
import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import GHC.Float
import qualified Graphics.Gloss as Gloss
import Debug.Trace
import Control.Parallel.Strategies
import GHC.Generics

-- global units
type GU = Double

focalLength :: GU
focalLength = 1

gameSize = 300 -- scales the gloss picture

-- canvas units
type CU = Float

-- | Convert global units to canvas units
gu2cu :: GU -> CU
gu2cu = (*5) . double2Float

type Radian = Double

data Coord a = Coord { x :: !a, y :: !a, z :: !a } deriving (Generic, Eq, Ord, Functor, Show)

-- 2d canvas coordinate
type CCoord = Gloss.Point

type Point = Coord GU

type Vect = Point

type Rotation = Coord Radian

-- | A tree of connected verticies, doesn't contain cycles
data VertTree a = Vertex a [VertTree a] | Leaf a deriving (Functor, Foldable, Show)

-- | builds a mesh from a list of bi-directional edges
meshFromEdges :: [(Point, Int, [Int])] -> Mesh Point
meshFromEdges edges = Mesh items tree where
  bounds = (0, length edges - 1)
  items = array bounds [ (k, v) | (v, k, _) <- edges ]
  keyMap = array bounds [ (k, ks) | (_, k, ks) <- edges ]
  (tree, _, _) = buildTree 0 (-1) IS.empty S.empty
  -- | Constructs a vertex tree where every branch is unique
  buildTree :: Int -> Int -> IS.IntSet -> S.Set (Int, Int) -> (VertTree Int, IS.IntSet, S.Set (Int, Int))
  buildTree i p visited es = (Vertex i ts, visited'', es'') where
    visited' = IS.insert i visited
    es' = S.insert (min i p, max i p) es
    children = filter (/= p) $ keyMap ! i
    (ts, visited'', es'') = foldr f ([], visited', es') children
    f i' k@(acc, visited, es)
      | S.member (min i' i, max i' i) es = k
      | IS.member i' visited = 
        (Leaf i' : acc, visited, S.insert (min i' i, max i' i) es)
      | otherwise = let (t, visited', es') = buildTree i' i visited es 
                    in (t:acc, visited', es')

-- | Transform a mesh into a list of Paths to be drawn by Gloss
-- The CCoord is Nothing if the point is behind the camera.
-- if drawing a line from a point behind the camera to one in front, find where
-- the line crosses the xy plane
projectedMeshToLines :: Mesh (Point, Maybe CCoord) -> [Gloss.Path]
projectedMeshToLines (Mesh v (Vertex i cs)) = withStrategy (parTraversable rdeepseq) $ concatMap (go $ v ! i) cs where
  go :: (Point, Maybe CCoord) -> VertTree Int -> [Gloss.Path]
  go (c, Just cCoord) (Vertex i cs)
    | Just pCoord <- mbCoord = [cCoord, pCoord] : concatMap (go p) cs
    | otherwise = [cCoord, intersect] : concatMap (go p) cs
    where
      p@(pc, mbCoord) = v ! i
      intersect = findIntersection pc c
  go (c, Nothing) (Vertex i cs)
    | Just pCoord <- mbCoord = [intersect, pCoord] : concatMap (go p) cs
    | otherwise = concatMap (go p) cs
    where
      p@(pc, mbCoord) = v ! i
      intersect = findIntersection pc c
  go (c, Nothing) (Leaf i)
    | Just pCoord <- mbCoord = [[intersect, pCoord]]
    | otherwise = []
    where
      (pc, mbCoord) = v ! i
      intersect = findIntersection pc c
  go (c, Just cCoord) (Leaf i)
    | Just pCoord <- mbCoord = [[cCoord, pCoord]]
    | otherwise = [[cCoord, intersect]]
    where
      (pc, mbCoord) = v ! i
      intersect = findIntersection pc c

  findIntercept (ax, ay) (bx, by) = gu2cu $ by - (by - ay) / (bx - ax) * bx
  findIntersection (Coord ax ay az) (Coord bx by bz) = (ix, iy) where
    iy = findIntercept (bz - focalLength, by) (az - focalLength, ay)
    ix = findIntercept (bz - focalLength, bx) (az - focalLength, ax)


-- withStrategy (parTraversable rdeepseq)
renderMeshes :: Camera -> [Mesh Point] -> Gloss.Picture
renderMeshes cam meshes = 
  Gloss.Color Gloss.blue 
  . Gloss.scale gameSize gameSize 
  . Gloss.Pictures 
  . map (Gloss.pictures . map Gloss.line) 
  . withStrategy (parTraversable rdeepseq) 
  $ map projectedMeshToLines (perspectiveTransform cam meshes)

data Mesh a = Mesh (Array Int a) (VertTree Int) deriving Show

instance Functor Mesh where
  fmap f (Mesh a t) = Mesh (fmap f a) t
instance Foldable Mesh where
  foldr f z (Mesh a t) = foldr f z a
instance Traversable Mesh where
  traverse f (Mesh a t) = flip Mesh t <$> (traverse f a)

data Camera = Camera { camLoc :: Point, camRot :: Rotation }

-- vectors are monoids
instance Num a => Semigroup (Coord a) where
  (Coord x1 y1 z1) <> (Coord x2 y2 z2) = Coord (x1 + x2) (y1 + y2) (z1 + z2)
instance Num a => Monoid (Coord a) where
  mempty = Coord 0 0 0
  mappend = (<>)
instance Applicative Coord where
  pure x = Coord x x x
  (Coord fx fy fz) <*> (Coord x y z) = Coord (fx x) (fy y) (fz z)
instance Num a => Num (Coord a) where
  (+) = (<>)
  a * b = (*) <$> a <*> b
  abs = fmap abs
  signum = fmap signum
  fromInteger x = fmap fromInteger $ Coord x x x
  negate = fmap negate
instance NFData a => NFData (Coord a)
  
-- | Takes a list of meshes and projects all the points in each one
-- into the display screen coordinate space.
-- Coordinates are Nothing if the point is behind the screen
perspectiveTransform :: (Functor f, Traversable f) => Camera -> f (Mesh Point) -> f (Mesh (Point, Maybe CCoord))
perspectiveTransform cam@Camera{..} =
  fmap (withStrategy (parTraversable rdeepseq) . fmap pers) . rotateMeshes mempty camRot . fmap (translatePoints (negate camLoc) 1) where
    pers p@(Coord x' y' z')
      | z' >= focalLength = (p, Just (bx, by))
      | otherwise = (p, Nothing)
      where
        fz = focalLength / z'
        bx = gu2cu $ fz * x'
        by = gu2cu $ fz * y'

-- | Translate the camera along a unit vector.
translateCam :: Vect -> GU -> Camera -> Camera
translateCam uv d cam = cam{camLoc = translatePoint uv d (camLoc cam)}

-- | Translate a point along a unit vector
translatePoint :: Vect -> GU -> Point -> Point
translatePoint uv d = mappend (fmap (*d) uv)

-- | Translates multiple points along a unit vector
translatePoints :: Functor f => Vect -> GU -> f Point -> f Point
translatePoints uv d = fmap $ translatePoint uv d

-- | Add a rotation vector to the camera's current rotation.
rotateCam :: Rotation -> Camera -> Camera
rotateCam v cam = cam{ camRot = wrap <$> camRot cam <> v } where
  wrap r | r < negate pi = pi - mod' (negate r) pi
         | r > pi = negate pi + mod' r pi
         | otherwise = r

-- | Rotate a vector
rotateVect :: Rotation -> Vect -> Vect
rotateVect r = rotatePoint mempty r

-- | right hand version
rotateVectR :: Rotation -> Vect -> Vect
rotateVectR r = rotatePointR mempty r

-- | Rotate a point around a pivot
rotatePoint :: Point -> Rotation -> Point -> Point
rotatePoint pivot r = head . rotatePoints pivot r . pure

-- | right hand version
rotatePointR :: Point -> Rotation -> Point -> Point
rotatePointR pivot r = head . rotatePointsR pivot r . pure

-- | Rotate some points around a pivot
rotatePoints :: (Functor f, Traversable f) => Point -> Rotation -> f Point -> f Point
rotatePoints pivot r = head . rotateMeshes pivot r . pure

rotateMeshes :: (Functor f, Functor g, Traversable g) => Point -> Rotation -> f (g Point) -> f (g Point)
rotateMeshes pivot r = fmap (fmap (mappend pivot . rotate . subtract pivot)) where
  [sx, sy, sz, cx, cy, cz] = [(t . f) r | t <- [sin, cos], f <- [x, y, z]]
  rotate (Coord x' y' z') = Coord dx dy dz where
    dx = cy * (sz * y' + cz * x') - sy * z'
    dy = sx * (cy * z' + sy * (sz * y' + cz * x')) + cx * (cz * y' - sz * x')
    dz = cx * (cy * z' + sy * (sz * y' + cz * x')) - sx * (cz * y' - sz * x')

-- | Right hand version
rotatePointsR :: Functor f => Point -> Rotation -> f Point -> f Point
rotatePointsR pivot r = fmap $ mappend pivot . rotate . subtract pivot where
  [sx, sy, sz, cx, cy, cz] = [(t . f) r | t <- [sin, cos], f <- [x, y, z]]
  rotate (Coord x' y' z') = Coord dx dy dz where
    dx = x' * cz * cy + y' * (sz * cx + cz * sy * sx) + z' * (sz * sx - cz * sy * cx)
    dy = -x' * sz * cy + y' * (cz * cx - sz * sy * sx) + z' * (cz * sx + sz * sy * cx)
    dz = x' * sy - y' * cy * sx + z' * cy * cx

-- | Normalize a vector and also get the magnitude of the original vector
normalizeVector :: Vect -> (Vect, GU)
normalizeVector (Coord 0 0 0) = (Coord 0 0 0, 0)
normalizeVector v = let m = distance (Coord 0 0 0) v in (fmap (/ m) v, m)

scalePoints :: Functor f => Point -> Double -> f Point -> f Point
scalePoints c scale = fmap f where
  f pt = translatePoint nv (d * scale) pt where
    (nv, d) = normalizeVector $ pt - c

-- | Find the distance between two points.
distance :: Point -> Point -> GU
distance (Coord x1 y1 z1) (Coord x2 y2 z2) 
  | yDiff == 0 = d
  | xDiff == 0 = sqrt $ yDiff ^ 2 + zDiff ^ 2
  | zDiff == 0 = sqrt $ xDiff ^ 2 + yDiff ^ 2
  | otherwise = sqrt $ yDiff ^ 2 + d ^ 2
  where
    yDiff = y1 - y2
    xDiff = x1 - x2
    zDiff = z1 - z2
    d = sqrt $ xDiff ^ 2 + zDiff ^ 2

getCenter :: (Foldable f, Functor f, Fractional a) => f (Coord a) -> Coord a
getCenter cs = div' $ foldr f (mempty, 0) cs where
  f x (acc, s) = (acc <> x, s + 1)
  div' (p, t) = (/ t) <$> p
