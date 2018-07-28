{-# language RecordWildCards #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}

-- https://jsfiddle.net/gwbse86v/17/

module Lib
    ( Coord (..)
    , GU
    , Point
    , Vect
    , Mesh (..)
    , Vertex
    , Camera (..)
    , perspectiveTransform
    , translateCam
    , rotateCam
    , rotateVect
    , rotatePoint
    ) where

import Control.Monad (guard)
import Data.Semigroup ((<>))
import Data.Maybe (catMaybes)
import Data.Fixed (mod')
import Data.Foldable

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

data Coord a = Coord { x :: a, y :: a, z :: a } deriving (Eq, Show, Ord, Functor)

data CCoord = CCoord { cx :: CU, cy :: CU }

type Point = Coord GU

type Vect = Point

type Rotation = Coord Radian

data Mesh a = Mesh a [Mesh a] deriving (Eq, Show, Functor)

type Vertex = Mesh Point


data Camera = Camera { camLoc :: Point, camRot :: Rotation }

-- vectors are monoids
instance Num a => Semigroup (Coord a) where
  (Coord x1 y1 z1) <> (Coord x2 y2 z2) = Coord (x1 + x2) (y1 + y2) (z1 + z2)
instance Num a => Monoid (Coord a) where
  mempty = Coord 0 0 0
  mappend = (<>)
  
-- | Takes a list of Vertices and projects all the points in each one
-- into the display screen coordinate space.
-- Coordinates are Nothing if the point is behind the screen
perspectiveTransform :: Camera -> [Vertex] -> [Mesh (Maybe CCoord)]
perspectiveTransform cam@Camera{..} = map (fmap transform) where
  [sx, sy, sz, cx, cy, cz] = [(t . f) camRot | t <- [sin, cos], f <- [x, y, z]]
  transform :: Point -> Maybe CCoord
  transform pt 
    | dz >= focalLength = Just $ CCoord bx by
    | otherwise = Nothing -- if dz is negative, point is behind the camera
    where
      (Coord x' y' z') = pt <> fmap negate camLoc
      dx = cy * (sz * y' + cz * x') - sy * z'
      dy = sz * (cy * z' + sy * (sz * y' + cz * x')) + cx * (cz * y' - sz * x')
      dz = cx * (cy * z' + sy * (sz * y' + cz * x')) - sx * (cz * y' - sz * x')
      bx = focalLength / dz * dx
      by = focalLength / dz * dy

-- | Translate the camera along a unit vector.
translateCam :: Camera -> Vect -> GU -> Camera
translateCam cam v d = cam{camLoc = camLoc cam <> fmap (*d) v}

-- | Add a rotation vector to the camera's current rotation.
rotateCam :: Camera -> Rotation -> Camera
rotateCam cam v = cam{camRot = wrap <$> camRot cam <> v} where
  wrap r | r < negate pi = pi - mod' (negate r) pi
         | r > pi = negate pi + mod' r pi

-- | Rotate a vector
rotateVect :: Vect -> Rotation -> Vect
rotateVect (Coord x' y' z') r = Coord dx dy dz where
  [sx, sy, sz, cx, cy, cz] = [(t . f) r | t <- [sin, cos], f <- [x, y, z]]
  dx = cy * (sz * y' + cz * x') - sy * z'
  dy = sz * (cy * z' + sy * (sz * y' + cz * x')) + cx * (cz * y' - sz * x')
  dz = cx * (cy * z' + sy * (sz * y' + cz * x')) - sx * (cz * y' - sz * x')

-- | Rotate a point about a pivot
rotatePoint :: Point -> Point -> Rotation -> Point
rotatePoint target pivot r = rotateVect (target <> fmap negate pivot) r <> pivot

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
