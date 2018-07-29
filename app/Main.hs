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
  , keys :: S.Set Key
  , picture :: Maybe Picture
  }

draw :: World -> Picture
draw = fromJust . picture 

drawMesh :: Mesh (Maybe CCoord) -> Picture
drawMesh = pictures . map line . projectedMeshToLines

handle :: Event -> World -> World
handle _ = id

update :: Float -> World -> World
update time world@World{..}
  | isJust picture = world
  | otherwise = world{ picture = Just pic }
  where
    pic = scale gameSize gameSize . Pictures $ map drawMesh (perspectiveTransform camera meshes)

main :: IO ()
main = do
  play display backColor fps world draw handle update
 where
  display = InWindow "3d" (windowWidth, windowHeight) (200, 200)
  backColor = white
  fps = 60
  world = World [fromJust $ sphere 1 6 4] (Camera (Coord 0 0 (-5)) (Coord 0 0 0)) S.empty Nothing
