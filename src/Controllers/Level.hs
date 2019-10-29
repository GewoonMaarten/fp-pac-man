module Controllers.Level where

import           Graphics.Gloss
import           Model

initialGrid :: [[Int]]
initialGrid =
  [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
  , [1, 4, 4, 4, 4, 4, 4, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 1]
  , [1, 4, 1, 1, 4, 1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 1, 4, 1]
  , [1, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 1]
  , [1, 4, 1, 1, 4, 1, 4, 1, 1, 1, 1, 1, 4, 1, 4, 1, 1, 4, 1]
  , [1, 4, 4, 4, 4, 1, 4, 4, 4, 1, 4, 4, 4, 1, 4, 4, 4, 4, 1]
  , [1, 1, 1, 1, 4, 1, 1, 1, 0, 1, 0, 1, 1, 1, 4, 1, 1, 1, 1]
  , [0, 0, 0, 1, 4, 1, 0, 0, 0, 0, 0, 0, 0, 1, 4, 1, 0, 0, 0]
  , [1, 1, 1, 1, 4, 1, 0, 1, 1, 3, 1, 1, 0, 1, 4, 1, 1, 1, 1]
  , [4, 4, 4, 4, 4, 4, 0, 1, 2, 2, 2, 1, 0, 4, 4, 4, 4, 4, 4]
  , [1, 1, 1, 1, 4, 0, 0, 1, 1, 1, 1, 1, 0, 0, 4, 1, 1, 1, 1]
  , [0, 0, 0, 1, 4, 1, 0, 0, 0, 0, 0, 0, 0, 1, 4, 1, 0, 0, 0]
  , [1, 1, 1, 1, 4, 1, 0, 1, 1, 1, 1, 1, 0, 1, 4, 1, 1, 1, 1]
  , [1, 4, 4, 4, 4, 4, 4, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 1]
  , [1, 4, 1, 1, 4, 1, 1, 1, 4, 1, 4, 1, 1, 1, 4, 1, 1, 4, 1]
  , [1, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 4, 4, 1]
  , [1, 1, 4, 1, 4, 1, 4, 1, 1, 1, 1, 1, 4, 1, 4, 1, 4, 1, 1]
  , [1, 4, 4, 5, 4, 1, 4, 4, 4, 1, 4, 4, 4, 1, 4, 5, 4, 4, 1]
  , [1, 4, 1, 1, 1, 1, 1, 1, 4, 1, 4, 1, 1, 1, 1, 1, 1, 4, 1]
  , [1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1]
  , [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
  ]

wallColor, dotColor :: Picture -> Picture
wallColor = color (makeColorI 23 24 254 255)

dotColor = color (makeColorI 254 169 164 255)

drawItem :: Float -> GridItem -> Float -> Float -> Picture
-- Nothing
drawItem s Empty x y =
  translate x y $ color black $ rectangleSolid s s
-- Wall
drawItem s Wall x y =
  translate x y $ wallColor $ rectangleSolid s s
-- Door
drawItem s Door x y = let doorSize = s / 5 in
  translate x y $ dotColor $ rectangleSolid s doorSize
-- SpawnPoint
drawItem s SpawnPoint x y =
  translate x y $ color black $ rectangleSolid s s
-- Pac-Dot
drawItem s (Collectible Available PacDot) x y =
  translate x y $ dotColor $ rectangleSolid (s / 4) (s / 4)
-- Energizer
drawItem s (Collectible Available Energizer) x y =
  translate x y $ dotColor $ circleSolid (s / 4)
-- Fruit
drawItem s (Collectible Available Fruit) x y =
  translate x y $ color red $ circleSolid (s / 2)

-- Parse to int list to an grid
buildGrid :: [[Int]] -> Float -> Float -> Float -> Grid
buildGrid gridItems x y s = Grid (map buildRow gridItems) x y s
 where
  buildRow :: [Int] -> [GridItem]
  buildRow []       = []
  buildRow (1 : xs) = Wall : buildRow xs
  buildRow (2 : xs) = SpawnPoint : buildRow xs
  buildRow (3 : xs) = Door : buildRow xs
  buildRow (4 : xs) = Collectible Available PacDot : buildRow xs
  buildRow (5 : xs) = Collectible Available Energizer : buildRow xs
  buildRow (_ : xs) = Empty : buildRow xs

-- Parse the grid to a list of pictures with coordinates for an offset
-- ToDo: make this not so weird.
showGrid :: Grid -> [Picture]
showGrid grid = foldr showRow [(translate x y blank)] gridItems
  where
    showRow :: [GridItem] -> [Picture] -> [Picture]
    showRow gis ps@(Translate _ y2 _:_)  = foldr showGridItem [(translate x (y2 + s) blank)] gis ++ ps
    showGridItem :: GridItem -> [Picture] -> [Picture]
    showGridItem gi ps@(Translate x2 y2 _:_)  = drawItem s gi (x2 + s) y2 : ps
    x         = getGridX grid
    y         = getGridY grid
    s         = getGridSize grid 
    gridItems = getGridItems grid
