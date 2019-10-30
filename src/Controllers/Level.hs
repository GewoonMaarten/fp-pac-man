module Controllers.Level where

import           Graphics.Gloss
import           Model

wallColor, dotColor :: Picture -> Picture
wallColor = color (makeColorI 23 24 254 255)

dotColor = color (makeColorI 254 169 164 255)

drawItem :: Float -> GridItem -> Float -> Float -> Picture
-- Nothing
drawItem s Empty x y = translate x y $ color black $ rectangleSolid s s
-- Wall
drawItem s Wall  x y = translate x y $ wallColor $ rectangleSolid s s
-- Door
drawItem s Door x y =
  let doorSize = s / 5 in translate x y $ dotColor $ rectangleSolid s doorSize
-- SpawnPoint
drawItem s SpawnPoint x y = translate x y $ color black $ rectangleSolid s s
-- Pac-Dot
drawItem s (Collectible Available PacDot _) x y =
  translate x y $ dotColor $ rectangleSolid (s / 4) (s / 4)
-- Energizer
drawItem s (Collectible Available Energizer _) x y =
  translate x y $ dotColor $ circleSolid (s / 4)
-- Fruit
drawItem s (Collectible Available Fruit _) x y =
  translate x y $ color red $ circleSolid (s / 2)
-- Nothing
drawItem s _ x y = translate x y $ color black $ rectangleSolid s s

-- Parse to int list to an grid
buildGrid :: Float -> Float -> Float -> Grid
buildGrid = Grid (map buildRow initialGrid)
 where
  buildRow :: [Int] -> [GridItem]
  buildRow []       = []
  buildRow (1 : xs) = Wall : buildRow xs
  buildRow (2 : xs) = SpawnPoint : buildRow xs
  buildRow (3 : xs) = Door : buildRow xs
  buildRow (4 : xs) = Collectible Available PacDot 10 : buildRow xs
  buildRow (5 : xs) = Collectible Available Energizer 50 : buildRow xs
  buildRow (_ : xs) = Empty : buildRow xs
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
    , [4, 4, 4, 4, 4, 0, 0, 1, 2, 2, 2, 1, 0, 0, 4, 4, 4, 4, 4]
    , [1, 1, 1, 1, 4, 1, 0, 1, 1, 1, 1, 1, 0, 1, 4, 1, 1, 1, 1]
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

-- Parse the grid to a list of pictures with coordinates for an offset
showGrid :: Grid -> [Picture]
showGrid g =
  let x         = getGridX g
      y         = getGridY g
      s         = getGridSize g
      gridItems = getGridItems g
  in  showGrid' gridItems x y s
 where
  showGrid' :: [[GridItem]] -> Float -> Float -> Float -> [Picture]
  showGrid' [] _ _ _ = []
  showGrid' (gis : giss) x y s =
    showRow gis x y s ++ showGrid' giss x (y - s) s
  showRow :: [GridItem] -> Float -> Float -> Float -> [Picture]
  showRow []        _  _  _ = []
  showRow (g : gis) x1 y1 s = drawItem s g x1 y1 : showRow gis (x1 + s) y1 s
