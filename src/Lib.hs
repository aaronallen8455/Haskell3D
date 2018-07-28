{-# language RecordWildCards #-}
{-# language DeriveFunctor #-}

-- https://jsfiddle.net/gwbse86v/17/

module Lib
    ( Coord (..)
    , GU
    , Radian
    , Rotation
    , CCoord
    , Point
    , Vect
    , Mesh
    , meshFromEdges
    , Camera (..)
    , perspectiveTransform
    , translateCam
    , translatePoint
    , translatePoints
    , rotateCam
    , rotateVect
    , rotatePoint
    , rotatePoints
    , distance
    ) where

import Control.Monad (guard)
import Data.Semigroup ((<>))
import Data.Maybe (catMaybes)
import Data.Fixed (mod')
import Data.Foldable
import Data.Array

-- global units
type GU = Double

focalLength :: GU
focalLength = 1

-- canvas units
type CU = Double

displayWidth :: CU
displayWidth = 700
displayHeight :: CU
displayHeight = 550

type Radian = Double

data Coord a = Coord { x :: a, y :: a, z :: a } deriving (Eq, Ord, Functor)

-- 2d canvas coordinate
data CCoord = CCoord { cx :: CU, cy :: CU }

type Point = Coord GU

type Vect = Point

type Rotation = Coord Radian

-- | Stores both the nodes and edges of a mesh
-- edges should be bi-directional
data Mesh a = Mesh { keyNode :: Array Int a, keyEdges :: Array Int [Int] }

instance Functor Mesh where
  fmap f (Mesh v k) = Mesh (fmap f v) k

meshFromEdges :: [(a, Int, [Int])] -> Mesh a
meshFromEdges edges = Mesh items edgeMap where
  bound = (0, length edges)
  items = array bound [(k, v) | (v, k, _) <- edges]
  edgeMap = array bound [(k, ks) | (_, k, ks) <- edges]

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
perspectiveTransform cam@Camera{..} = 
  fmap $ \m -> pers <$> rotatePoints m camLoc camRot where
    pers (Coord x' y' z')
      | z' >= focalLength = Just $ CCoord bx by
      | otherwise = Nothing
      where
        fz = focalLength / z'
        bx = fz * x'
        by = fz * y'

-- | Translate the camera along a unit vector.
translateCam :: Camera -> Vect -> GU -> Camera
translateCam cam uv d = cam{camLoc = translatePoint (camLoc cam) uv d}

-- | Translate a point along a unit vector
translatePoint :: Point -> Vect -> GU -> Point
translatePoint p uv d = head $ translatePoints [p] uv d

translatePoints :: Functor f => f Point -> Vect -> GU -> f Point
translatePoints m uv d = fmap (\p -> translatePoint p uv d) m

-- | Add a rotation vector to the camera's current rotation.
rotateCam :: Camera -> Rotation -> Camera
rotateCam cam v = cam{ camRot = wrap <$> camRot cam <> v } where
  wrap r | r < negate pi = pi - mod' (negate r) pi
         | r > pi = negate pi + mod' r pi

-- | Rotate a vector
rotateVect :: Vect -> Rotation -> Vect
rotateVect v r = rotatePoint v mempty r

-- | Rotate a point around a pivot
rotatePoint :: Point -> Point -> Rotation -> Point
rotatePoint target pivot r = head $ rotatePoints [target] pivot r

-- | Rotate some points around a pivot
rotatePoints :: Functor f => f Point -> Point -> Rotation -> f Point
rotatePoints pts pivot r = mappend pivot . rotate . mappend (fmap negate pivot) <$> pts where
  [sx, sy, sz, cx, cy, cz] = [(t . f) r | t <- [sin, cos], f <- [x, y, z]]
  rotate (Coord x' y' z') = Coord dx dy dz where
    dx = cy * (sz * y' + cz * x') - sy * z'
    dy = sz * (cy * z' + sy * (sz * y' + cz * x')) + cx * (cz * y' - sz * x')
    dz = cx * (cy * z' + sy * (sz * y' + cz * x')) - sx * (cz * y' - sz * x')

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
