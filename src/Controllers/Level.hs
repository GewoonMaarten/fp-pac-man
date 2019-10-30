module Controllers.Level
  ( buildGrid
  , showGrid
  )
where

import           Graphics.Gloss
import           Model

wallColor, dotColor :: Picture -> Picture
wallColor = color (makeColorI 23 24 254 255)

dotColor = color (makeColorI 254 169 164 255)

drawItem :: GridItem -> Picture
-- Empty
drawItem (Empty (h, w) (x, y)) =
  translate x y $ color black $ rectangleSolid h w
-- Wall
drawItem (Wall (h, w) (x, y)) = translate x y $ wallColor $ rectangleSolid h w
-- Door
drawItem (Door (h, w) (x, y)) =
  let doorSize = h / 5 in translate x y $ dotColor $ rectangleSolid w doorSize
-- SpawnPoint
drawItem (SpawnPoint (h, w) (x, y)) =
  translate x y $ color black $ rectangleSolid w h
-- Pac-Dot
drawItem (Collectible (x, y) Available (PacDot (w, h))) =
  translate x y $ dotColor $ rectangleSolid w h
-- Energizer
drawItem (Collectible (x, y) Available (Energizer r)) =
  translate x y $ dotColor $ circleSolid r
-- Fruit
drawItem (Collectible (x, y) Available (Fruit r)) =
  translate x y $ color red $ circleSolid r
-- Collected Pac-Dot 
drawItem (Collectible (x, y) Collected (PacDot (w, h))) =
  translate x y $ color black $ rectangleSolid w h
-- Collected Energizer
drawItem (Collectible (x, y) Collected (Energizer r)) =
  translate x y $ color black $ circleSolid r
-- Collected Fruit
drawItem (Collectible (x, y) Collected (Fruit r)) =
  translate x y $ color black $ circleSolid r

-- Parse to int list to an grid
buildGrid :: Float -> Float -> Float -> Grid
buildGrid x y s = Grid (buildGrid' initialGrid x y) x y s
 where
  buildGrid' :: [[Int]] -> Float -> Float -> [[GridItem]]
  buildGrid' []           _ _ = []
  buildGrid' (gis : giss) x y = buildRow gis x y : buildGrid' giss x (y - s)
  buildRow :: [Int] -> Float -> Float -> [GridItem]
  buildRow []        _ _ = []
  buildRow (1 : gis) x y = Wall (s, s) (x, y) : buildRow gis (x + s) y
  buildRow (2 : gis) x y = SpawnPoint (s, s) (x, y) : buildRow gis (x + s) y
  buildRow (3 : gis) x y = Door (s, s) (x, y) : buildRow gis (x + s) y
  buildRow (4 : gis) x y =
    let smallS = s / 4
    in  Collectible (x, y) Available (PacDot (smallS, smallS))
          : buildRow gis (x + s) y
  buildRow (5 : gis) x y =
    Collectible (x, y) Available (Energizer (s / 4)) : buildRow gis (x + s) y
  buildRow (_ : gis) x y = Empty (s, s) (x, y) : buildRow gis (x + s) y
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
showGrid = map drawItem . concat . getGridItems
