{-# language RecordWildCards #-}

module Main where

import Lib
import Primitives
import Graphics.Gloss hiding (Point, circle)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, circle)
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

gameSize        = 300
windowWidth     = 1200
windowHeight    = 900

data World = World 
  { meshes :: [Mesh Point]
  , camera :: Camera
  , keys :: S.Set Key
  , picture :: Maybe Picture
  }

draw :: World -> Picture
draw World{..}
  | (Just pic) <- picture = pic
  | otherwise = renderMeshes camera meshes

drawMesh :: Mesh (Maybe CCoord) -> Picture
drawMesh = pictures . map line . projectedMeshToLines

renderMeshes :: Camera -> [Mesh Point] -> Picture
renderMeshes cam meshes = Color blue . scale gameSize gameSize . Pictures $ map drawMesh (perspectiveTransform cam meshes)

handle :: Event -> World -> World
handle (EventKey k s _ _) world@World{..} = world{ picture = pic, keys = keys' } where
  keys' | s == Down = S.insert k keys
        | otherwise = S.delete k keys
  pic | S.null keys' = picture
      | otherwise = Nothing
handle _ x = x

transStep = 0.3
rotStep = pi / 300

update :: Float -> World -> World
update time world@World{..}
  | isJust picture = world
  | otherwise = world{ camera = cam' }
  where
    (Camera loc rot) = camera
    -- do camera transformations
    l = if S.member (Char 's') keys then Coord (-1) 0 0 else mempty
    r = if S.member (Char 'f') keys then Coord 1 0 0 else mempty
    f = if S.member (Char 'e') keys then Coord 0 0 1 else mempty
    b = if S.member (Char 'd') keys then Coord 0 0 (-1) else mempty
    u = if S.member (Char 'z') keys then Coord 0 1 0 else mempty
    d = if S.member (Char 'v') keys then Coord 0 (-1) 0 else mempty
    pr = if S.member (SpecialKey KeyRight) keys then Coord 0 rotStep 0 else mempty
    pl = if S.member (SpecialKey KeyLeft) keys then Coord 0 (-rotStep) 0 else mempty
    pd = if S.member (SpecialKey KeyDown) keys then Coord rotStep 0 0 else mempty
    pu = if S.member (SpecialKey KeyUp) keys then Coord (-rotStep) 0 0 else mempty
    rl = if S.member (Char 'w') keys then Coord 0 0 rotStep else mempty
    rr = if S.member (Char 'r') keys then Coord 0 0 (-rotStep) else mempty
    totalRot = mconcat [pl, pr, pu, pd, rl, rr]
    cam@(Camera _ rot') = rotateCam totalRot camera
    totalTrans = mconcat [l, r, f, b, u, d]
    vect = rotateVectRh (fmap negate rot') . fst $ normalizeVector totalTrans
    cam' = translateCam vect transStep cam

main :: IO ()
main = do
  play display backColor fps world draw handle update
 where
  display = InWindow "3d" (windowWidth, windowHeight) (200, 200)
  backColor = white
  fps = 60
  world = World [sph, translatePoints (Coord 1 0 0) 5 sph] (Camera (Coord 0 0 (-10)) (Coord 0 0 0)) S.empty Nothing

uc = meshFromEdges [(Coord 0 0 0, 0, [1,3,4])
                       ,(Coord 0 0 1, 1, [0,2,5])
                       ,(Coord 1 0 1, 2, [1,3,6])
                       ,(Coord 1 0 0, 3, [0,2,7])
                       ,(Coord 0 1 0, 4, [5,7,0])
                       ,(Coord 0 1 1, 5, [4,6,1])
                       ,(Coord 1 1 1, 6, [5,7,2])
                       ,(Coord 1 1 0, 7, [6,4,3])]
(Just circ) = circle 1 6

(Just sph) = sphere 1 20 18