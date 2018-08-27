{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Matrix                        hiding (trace)
import           Data.Maybe
import qualified Data.Set                           as S
import qualified Data.Vector                        as V
import           Debug.Trace
import           Graphics.Gloss                     hiding (Point, circle)
import           Graphics.Gloss.Interface.Pure.Game hiding (Point, circle)
import           Lib
import           Primitives

windowWidth     = 1200
windowHeight    = 900

data World = World
  { meshes  :: [Mesh Point]
  , camera  :: Camera
  , keys    :: S.Set Key
  , picture :: Maybe Picture
  }

draw :: World -> Picture
draw World{..}
  | (Just pic) <- picture = pic
  | otherwise = renderMeshes camera meshes

handle :: Event -> World -> World
handle (EventKey k s _ _) world@World{..} = world{ picture = pic, keys = keys' } where
  keys' | s == Down = S.insert k keys
        | otherwise = S.delete k keys
  pic | S.null keys' = picture
      | otherwise = Nothing
handle _ x = x

transStep = 0.3 :: GU
rotStep = pi / 300 :: Radian

update :: Float -> World -> World
update time world@World{..}
  | S.null keys && isJust picture = world
  | otherwise = world{ camera = cam', picture = Just pic }
  where
    (Camera loc rot) = camera
    -- do camera transformations
    empty = coord 0 0 0
    l = if S.member (Char 's') keys then coord (-1) 0 0 else empty
    r = if S.member (Char 'f') keys then coord 1 0 0 else empty
    f = if S.member (Char 'e') keys then coord 0 0 1 else empty
    b = if S.member (Char 'd') keys then coord 0 0 (-1) else empty
    u = if S.member (Char 'z') keys then coord 0 1 0 else empty
    d = if S.member (Char 'v') keys then coord 0 (-1) 0 else empty
    pr = if S.member (SpecialKey KeyRight) keys then coord 0 (-rotStep) 0 else empty
    pl = if S.member (SpecialKey KeyLeft) keys then coord 0 rotStep 0 else empty
    pd = if S.member (SpecialKey KeyDown) keys then coord (-rotStep) 0 0 else empty
    pu = if S.member (SpecialKey KeyUp) keys then coord rotStep 0 0 else empty
    rl = if S.member (Char 'w') keys then coord 0 0 (-rotStep) else empty
    rr = if S.member (Char 'r') keys then coord 0 0 rotStep else empty
    totalRot = foldr1 (+) [pl, pr, pu, pd, rl, rr]
    cam@(Camera _ rot') = rotateCam totalRot camera
    totalTrans = foldr1 (+) [l, r, f, b, u, d]
    vect = fst $ normalizeVector totalTrans
    cam' = translateCam vect transStep cam
    pic = renderMeshes cam' meshes

main :: IO ()
main = play display backColor fps world draw handle update
 where
  display = InWindow "3d" (windowWidth, windowHeight) (200, 200)
  backColor = dark . dark . dark $ dark blue
  fps = 60
  world = World [
    --scalePoints (coord 0 0.5 0) 2 sph,
    --translatePoints (coord 0 1 0) 5 sph,
    --translatePoints (coord 0 0 1) 5 sph,
    --translatePoints (coord 0 0 2) 5 sph,
    translatePoints (coord 1 0 0) 5 sph,
    translatePoints (coord 0 1 0) 15 tor,
    translatePoints (coord 0 (-1) 0) 5 sph,
    --bx,
    dod]
    (Camera (identity 4) (identity 4))
    S.empty
    Nothing

uc = meshFromEdges [(coord 0 0 0, 0, [1,3,4])
                   ,(coord 0 0 1, 1, [0,2,5])
                   ,(coord 1 0 1, 2, [1,3,6])
                   ,(coord 1 0 0, 3, [0,2,7])
                   ,(coord 0 1 0, 4, [5,7,0])
                   ,(coord 0 1 1, 5, [4,6,1])
                   ,(coord 1 1 1, 6, [5,7,2])
                   ,(coord 1 1 0, 7, [6,4,3])]
(Just circ) = circle 1 6

(Just sph) = sphere 1 15 13

(Just tor) = torus 1.5 5 14 14

(Just pl) = plane 5 5 5 5

(Just bx) = box 5 5 5 2 2 2

(Just dod) = dodecahedron 1
