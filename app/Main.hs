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
windowWidth     = 800
windowHeight    = 600

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
handle _ = id

update :: Float -> World -> World
update time world@World{..}
  | isJust picture = world
  | otherwise = world{ picture = Just pic }
  where
    pic = renderMeshes camera meshes

main :: IO ()
main = do
  play display backColor fps world draw handle update
 where
  display = InWindow "3d" (windowWidth, windowHeight) (200, 200)
  backColor = white
  fps = 1
  world = World [sph] (Camera (Coord 0.5 0.5 (-30)) (Coord 0 0 0)) S.empty Nothing

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