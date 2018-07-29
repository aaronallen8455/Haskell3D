{-# language RecordWildCards #-}

module Main where

import Lib
import Primitives
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import qualified Data.Set as S
import Data.Maybe

gameSize        = 300
windowWidth     = 800
windowHeight    = 600

data World = World 
  { meshes :: [Mesh Point]
  , camera :: Camera
  }

draw :: World -> Picture
draw world@World{..} = scale gameSize gameSize . Pictures $ map drawMesh (perspectiveTransform camera meshes) 

drawMesh :: Mesh (Maybe CCoord) -> Picture
drawMesh = pictures . map line . projectedMeshToLines

handle = undefined

update = undefined

main :: IO ()
main = do
  play display backColor fps world draw handle update
 where
  display = InWindow "3d" (windowWidth, windowHeight) (200, 200)
  backColor = blue
  fps = 60
  world = World [fromJust $ sphere 1 6 4] $ Camera (Coord 0 0 (-5)) (Coord 0 0 0)
