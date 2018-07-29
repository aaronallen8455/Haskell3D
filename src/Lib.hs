{-# language RecordWildCards #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}

-- https://jsfiddle.net/gwbse86v/17/

module Lib
    ( Coord (..)
    , GU
    , CU
    , Radian
    , Rotation
    , CCoord
    , Point
    , Vect
    , Mesh (..)
    , meshFromEdges
    , projectedMeshToLines
    , Camera (..)
    , perspectiveTransform
    , translateCam
    , translatePoint
    , translatePoints
    , rotateCam
    , rotateVect
    , rotateVectRh
    , rotatePoint
    , rotatePoints
    , distance
    , scalePoints
    , normalizeVector
    , getCenter
    ) where

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
import qualified Graphics.Gloss as Gloss (Point, Path)
import Debug.Trace

-- global units
type GU = Double

focalLength :: GU
focalLength = 1

-- canvas units
type CU = Float

gu2cu :: GU -> CU
gu2cu = (*10) . double2Float

type Radian = Double

data Coord a = Coord { x :: a, y :: a, z :: a } deriving (Eq, Ord, Functor, Show)

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

-- | Transform a mesh of CCoords into a list of Paths to be drawn by Gloss
projectedMeshToLines :: Mesh (Maybe CCoord) -> [Gloss.Path]
projectedMeshToLines (Mesh v t) = go Nothing t where
  go :: Maybe Int -> VertTree Int -> [Gloss.Path]
  go Nothing (Vertex i cs) = concatMap (go (Just i)) cs
  go (Just p) (Vertex i cs)
    | (Just a) <- vp, (Just b) <- vi =
      [a, b] : concatMap (go (Just i)) cs
    | isJust vi = concatMap (go (Just i)) cs
    | otherwise = concatMap (go Nothing) cs
    where
      vp = v ! p
      vi = v ! i
  go Nothing (Leaf _) = []
  go (Just p) (Leaf i)
    | (Just a) <- vp, (Just b) <- vi = [[a, b]]
    | otherwise = []
    where
      vp = v ! p
      vi = v ! i


data Mesh a = Mesh (Array Int a) (VertTree Int) deriving Show

instance Functor Mesh where
  fmap f (Mesh a t) = Mesh (fmap f a) t
instance Foldable Mesh where
  foldr f z (Mesh a t) = foldr f z a

data Camera = Camera { camLoc :: Point, camRot :: Rotation }

-- vectors are monoids
instance Num a => Semigroup (Coord a) where
  (Coord x1 y1 z1) <> (Coord x2 y2 z2) = Coord (x1 + x2) (y1 + y2) (z1 + z2)
instance Num a => Monoid (Coord a) where
  mempty = Coord 0 0 0
  mappend = (<>)
  
-- | Takes a list of meshes and projects all the points in each one
-- into the display screen coordinate space.
-- Coordinates are Nothing if the point is behind the screen
perspectiveTransform :: Functor f => Camera -> f (Mesh Point) -> f (Mesh (Maybe CCoord))
perspectiveTransform cam@Camera{..} = -- need to only call rotatePoints once
  fmap $ fmap pers . rotatePoints mempty camRot . translatePoints (fmap negate camLoc) 1 where
    pers p@(Coord x' y' z')
      | z' >= focalLength = Just $ (bx, by)
      | otherwise = Nothing
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

rotateVectRh :: Rotation -> Vect -> Vect
rotateVectRh r = rotatePointRh mempty r

-- | Rotate a point around a pivot
rotatePoint :: Point -> Rotation -> Point -> Point
rotatePoint pivot r = head . rotatePoints pivot r . pure

rotatePointRh :: Point -> Rotation -> Point -> Point
rotatePointRh pivot r = head . rotatePointsRh pivot r . pure

-- | Rotate some points around a pivot
rotatePoints :: Functor f => Point -> Rotation -> f Point -> f Point
rotatePoints pivot r = fmap $ mappend pivot . rotate . mappend (fmap negate pivot) where
  [sx, sy, sz, cx, cy, cz] = [(t . f) r | t <- [sin, cos], f <- [x, y, z]]
  rotate (Coord x' y' z') = Coord dx dy dz where
    dx = cy * (sz * y' + cz * x') - sy * z'
    dy = sx * (cy * z' + sy * (sz * y' + cz * x')) + cx * (cz * y' - sz * x')
    dz = cx * (cy * z' + sy * (sz * y' + cz * x')) - sx * (cz * y' - sz * x')

rotatePointsRh :: Functor f => Point -> Rotation -> f Point -> f Point
rotatePointsRh pivot r = fmap $ mappend pivot . rotate . mappend (fmap negate pivot) where
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
  f pt= translatePoint nv (d * scale) pt where
    (nv, d) = normalizeVector $ pt <> fmap negate c

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
