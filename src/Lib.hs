{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import           Control.Monad               (foldM, guard)
import           Control.Parallel.Strategies
import           Data.Array
import           Data.Fixed                  (mod')
import           Data.Foldable               hiding (toList)
import qualified Data.IntSet                 as IS
import           Data.Matrix                 hiding (trace, (!))
import           Data.Maybe                  (catMaybes, isJust, isNothing)
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import           Debug.Trace
import           GHC.Float
import           GHC.Generics
import qualified Graphics.Gloss              as Gloss

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

-- data Coord a = Coord { x :: a, y :: a, z :: a } deriving (Generic, Eq, Ord, Functor, Show)
type Coord = Matrix Double

coord :: Double -> Double -> Double -> Coord
coord x y z = fromList 4 1 [x, y, z, 1]
getX = getElem 1 1
getY = getElem 2 1
getZ = getElem 3 1

-- 2d canvas coordinate
type CCoord = Gloss.Point

type Point = Coord

type Vect = Point

type Rotation = Coord

-- | Represents a mesh with an array of nodes and a tree which encodes
-- the edges between nodes.
data Mesh a = Mesh (Array Int a) (VertTree Int) deriving Show

instance Functor Mesh where
  fmap f (Mesh a t) = Mesh (fmap f a) t
instance Foldable Mesh where
  foldr f z (Mesh a t) = foldr f z a
instance Traversable Mesh where
  traverse f (Mesh a t) = flip Mesh t <$> (traverse f a)

type TrMatrix = Matrix GU
type RMatrix = Matrix GU
type TMatrix = Matrix GU

-- | Represents a viewpoint as a translation matrix and a transformation matrix
-- which is the rotation matrix multiplied with the translation matrix.
data Camera = Camera { camTranslation :: TMatrix, camTransformation :: TrMatrix }

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
projectedMeshToLines (Mesh v (Vertex i cs)) = concatMap (go $ v ! i) cs where
  go :: (Point, Maybe CCoord) -> VertTree Int -> [Gloss.Path]
  go (c, mbCCoord) (Vertex i cs) = case mbCCoord of
    Just cCoord -> case mbCoord of
      Just pCoord -> [cCoord, pCoord] : concatMap (go p) cs
      _           -> [cCoord, intersect] : concatMap (go p) cs
    _ -> case mbCoord of
      Just pCoord -> [intersect, pCoord] : concatMap (go p) cs
      _           ->concatMap (go p) cs
    where
      p@(pc, mbCoord) = v ! i
      intersect = findIntersection pc c
  go (c, mbCCoord) (Leaf i) = case mbCCoord of
    Just cCoord -> case mbCoord of
      Just pCoord -> [[cCoord, pCoord]]
      _           -> [[cCoord, intersect]]
    _ -> case mbCoord of
      Just pCoord -> [[intersect, pCoord]]
      _           -> []
    where
      p@(pc, mbCoord) = v ! i
      intersect = findIntersection pc c

  findIntercept (ax, ay) (bx, by) = gu2cu $ by - (by - ay) / (bx - ax) * bx
  findIntersection :: Coord -> Coord -> (Float, Float)
  findIntersection a b = (ix, iy) where
    iy = findIntercept (getZ b - focalLength, getY b) (getZ a - focalLength, getY a)
    ix = findIntercept (getZ b - focalLength, getX b) (getZ a - focalLength, getX a)

-- | Transforms a list of meshes into a Gloss Picture
renderMeshes :: Camera -> [Mesh Point] -> Gloss.Picture
renderMeshes cam =
  Gloss.Color Gloss.white
  . Gloss.scale gameSize gameSize
  . Gloss.pictures
  . map (Gloss.pictures . map Gloss.line)
  . withStrategy (parTraversable rdeepseq)
  . map projectedMeshToLines
  . perspectiveTransform cam

-- | Applies a transformation matrix to a point using matrix multiplication
applyMatrix :: Matrix GU -> Point -> Point
applyMatrix = multStd2

-- | Takes a list of meshes and projects all the points in each one
-- into the display screen coordinate space.
-- Coordinates are Nothing if the point is behind the screen
perspectiveTransform :: (Functor f, Traversable f) => Camera -> f (Mesh Point) -> f (Mesh (Point, Maybe CCoord))
perspectiveTransform cam@Camera{..} =
  fmap $ withStrategy (parTraversable rdeepseq) . fmap (pers . applyMatrix camTransformation) where
    pers p
      | getZ p >= focalLength = (p, Just (bx, by))
      | otherwise = (p, Nothing)
      where
        fz = focalLength / getZ p
        bx = gu2cu $ fz * getX p
        by = gu2cu $ fz * getY p

-- | Translate the camera along a unit vector.
translateCam :: Vect -> GU -> Camera -> Camera
translateCam dir d cam@Camera{..} = Camera newT newTR where
  -- extract orthagonal normal vectors
  [right, up, out] = [V.take 3 $ getRow x camTransformation | x <- [1..3]]
  dir' = fmap (*(-d)) dir
  right' = fmap (* getX dir') right
  up' = fmap (* getY dir') up
  out' = fmap (* getZ dir') out
  all = V.fromList [sum $ map (V.! i) [right', up', out'] | i <- [0..2]]
  -- make new translation matrix
  newT = mapCol colF 4 camTranslation
  colF 4 _  = 1
  colF ri v = v + all V.! (ri-1)
  -- extract rotation matrix
  rM = mapCol rcf 4 camTransformation
  rcf 4 _ = 1
  rcf _ _ = 0
  -- get new transformation matrix
  newTR = rM `multStd2` newT


-- | Translate a point along a unit vector
translatePoint :: Vect -> GU -> Point -> Point
translatePoint uv d = unsafeSet 1 (4, 1) . (+) (fmap (*d) uv)

-- | Translates multiple points along a unit vector
translatePoints :: Functor f => Vect -> GU -> f Point -> f Point
translatePoints uv d = fmap $ translatePoint uv d

xRotMatrix :: Radian -> RMatrix
xRotMatrix a = fromLists [
                           [1, 0 , 0  , 0],
                           [0, ca, -sa, 0],
                           [0, sa, ca , 0],
                           [0, 0 , 0  , 1]
                         ] where
                          ca = cos a
                          sa = sin a
yRotMatrix :: Radian -> RMatrix
yRotMatrix a = fromLists [
                           [ca , 0, sa, 0],
                           [0  , 1, 0 , 0],
                           [-sa, 0, ca, 0],
                           [0  , 0, 0 , 1]
                         ] where
                          sa = sin a
                          ca = cos a
zRotMatrix :: Radian -> RMatrix
zRotMatrix a = fromLists [
                           [ca, -sa, 0, 0],
                           [sa, ca , 0, 0],
                           [0 , 0  , 1, 0],
                           [0 , 0  , 0, 1]
                         ] where
                          ca = cos a
                          sa = sin a

-- change order?
getRotationMatrix :: Rotation -> RMatrix
getRotationMatrix r = foldl (multStd2) (identity 4) . map (uncurry ($)) . filter ((/= 0) . snd) $
  [(xRotMatrix, getX r), (yRotMatrix, getY r), (zRotMatrix, getZ r)]

-- | Add a rotation vector to the camera's current rotation.
rotateCam :: Rotation -> Camera -> Camera
rotateCam v cam@Camera{..} = cam{camTransformation = newTr} where
  newTr | v /= (coord 0 0 0) = rotM * camTransformation
        | otherwise = camTransformation
  rotM = getRotationMatrix v

-- | Rotate a vector
rotateVect :: Rotation -> Vect -> Vect
rotateVect r = rotatePoint (coord 0 0 0) r

-- | Rotate a point around a pivot
rotatePoint :: Point -> Rotation -> Point -> Point
rotatePoint pivot r = head . rotatePoints pivot r . pure

-- | Rotate some points around a pivot
rotatePoints :: (Functor f, Traversable f) => Point -> Rotation -> f Point -> f Point
rotatePoints pivot r = head . rotateMeshes pivot r . pure

rotateMeshes :: (Functor f, Functor g, Traversable g) => Point -> Rotation -> f (g Point) -> f (g Point)
rotateMeshes pivot r = fmap (fmap ((+) pivot . rotate . subtract pivot)) where
  rotation = getRotationMatrix r
  rotate pt = rotation `multStd2` pt

-- | Normalize a vector and also get the magnitude of the original vector
normalizeVector :: Vect -> (Vect, GU)
normalizeVector v
  | x == 0 && y == 0 && z == 0 = (v, 0)
  | otherwise = (coord (x / m) (y / m) (z / m), m)
  where
    x = getX v
    y = getY v
    z = getZ v
    m = distance (coord 0 0 0) v

scalePoints :: Functor f => Point -> Double -> f Point -> f Point
scalePoints c scale = fmap f where
  f pt = translatePoint nv (d * scale) pt where
    (nv, d) = normalizeVector $ pt - c

-- | Find the distance between two points.
distance :: Point -> Point -> GU
distance a b
  | yDiff == 0 = sqrt d
  | xDiff == 0 = sqrt $ yDiff ^ 2 + zDiff ^ 2
  | zDiff == 0 = sqrt $ xDiff ^ 2 + yDiff ^ 2
  | otherwise = sqrt $ yDiff ^ 2 + d
  where
    (x1:y1:z1:_) = toList a
    (x2:y2:z2:_) = toList b
    yDiff = y1 - y2
    xDiff = x1 - x2
    zDiff = z1 - z2
    d = xDiff ^ 2 + zDiff ^ 2

getCenter :: (Foldable f, Functor f) => f Coord -> Coord
getCenter cs = div' $ foldr f (coord 0 0 0, 0) cs where
  f x (acc, s) = (acc + x, s + 1)
  div' (p, t) = (/ t) <$> p
