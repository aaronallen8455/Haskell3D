{-# LANGUAGE RecordWildCards #-}

import VectorZipper
import Lib
import Primitives
import           Graphics.Gloss                     hiding (Point, circle)
import           Graphics.Gloss.Interface.Pure.Game hiding (Point, circle)
import qualified Data.Set as S
import Data.Maybe
import Data.Matrix hiding (trace, fromList)
import Debug.Trace

windowWidth     = 1200
windowHeight    = 900
conwayRate = 0.2

data World = World
  { meshes  :: [Mesh Point]
  , camera  :: Camera
  , keys    :: S.Set Key
  , picture :: Picture
  , lastUpdate :: Float
  , conway :: ZZZ Bool
  }

draw :: World -> Picture
draw World{..} = picture

handle :: Event -> World -> World
handle (EventKey k s _ _) world@World{..} = world{ keys = keys' } where
  keys' | s == Down = S.insert k keys
        | otherwise = S.delete k keys
handle _ x = x

transStep = 0.3 :: GU
rotStep = pi / 300 :: Radian

update :: Float -> World -> World
update time world@World{..} = world{ camera = cam', picture = pic, conway = conway', meshes = meshes', lastUpdate = lastUpdate' }
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
    -- update the conway universe if enough time has elapsed
    (conway', meshes', lastUpdate') | lastUpdate + time >= conwayRate = 
                         let c = lifeStep conway in (c, map makeMesh $ getCells c, lastUpdate + time - conwayRate)
                       | otherwise = (conway, meshes, lastUpdate + time)

    pic = Color white $ renderMeshes cam' meshes'

makeMesh :: (Int, Int, Int) -> Mesh Point
makeMesh (x, y, z) = translatePoints (coord x' y' z') 1 . fromJust $ box 1 1 1 0 0 0 where
  [x', y', z'] = map fromIntegral [x, y, z]

main :: IO ()
main = play display backColor fps world draw handle update
 where
  display = InWindow "3d" (windowWidth, windowHeight) (200, 200)
  backColor = dark . dark . dark $ dark blue
  fps = 60
  world = World []
    (Camera (identity 4) (identity 4))
    S.empty
    Blank
    (-1)
    initConway

initConway :: ZZZ Bool
initConway = fromList [[[S.member (x, y, z) glider | z <- [0..14]] | y <- [0..14]] | x <- [0..7] ]

glider = S.fromList [
    (0,0,0), (1,0,0),
    (0,1,0), (1,1,0),
    (0,2,0), (1,2,0),
    (0,2,1), (1,2,1),
    (0,1,2), (1,1,2),

    (3,4,3), (3,5,3),
    (3,4,4), (3,5,4),
    (3,4,5), (3,5,5),
    (4,4,5), (4,5,5),
    (5,4,4), (5,5,4)
  ]
