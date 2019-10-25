module Level where

import Graphics.Gloss

type Grid = [[GridItem]] 

data CollectibleType = PacDot 
    | Energizer 
    | Fruit 
    deriving (Show)

data CollectibleState = Collected 
    | Available 
    deriving (Show)

data GridItem = Empty 
    | Door
    | Wall 
    | SpawnPoint 
    | Collectible CollectibleState CollectibleType
    deriving (Show)

gridSize = 20

initGrid :: [[Int]]
initGrid = [[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
            [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1],
            [1,0,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,0,1],
            [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
            [1,0,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,0,1],
            [1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1],
            [1,1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1],
            [1,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,1,1,1],
            [1,1,1,1,0,1,0,1,1,3,1,1,0,1,0,1,1,1,1],
            [0,0,0,0,0,0,0,1,2,2,2,1,0,0,0,0,0,0,0],
            [1,1,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,1],
            [1,1,1,1,0,1,0,0,0,0,0,0,0,1,0,1,1,1,1],
            [1,1,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,1],
            [1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1],
            [1,0,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,0,1],
            [1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1],
            [1,1,0,1,0,1,0,1,1,1,1,1,0,1,0,1,0,1,1],
            [1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1],
            [1,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,1],
            [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
            [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]

wallColor = color (makeColorI 23 24 254 255)
dotColor  = color (makeColorI 254 169 164 255)

class Drawable a where
    drawItem :: a -> Float -> Float -> Picture

instance Drawable GridItem where
    -- Nothing
    drawItem Empty x y = translate x y 
        $ color black 
        $ rectangleSolid gridSize gridSize
    -- Wall
    drawItem Wall x y = translate x y 
        $ wallColor 
        $ rectangleSolid gridSize gridSize
    -- Door
    drawItem Door x y = translate x y 
        $ color black 
        $ rectangleSolid gridSize gridSize
    -- SpawnPoint
    drawItem SpawnPoint x y = translate x y 
        $ color black 
        $ rectangleSolid gridSize gridSize
    -- Pac-Dot
    drawItem (Collectible Available PacDot) x y = translate x y 
        $ dotColor 
        $ rectangleSolid (gridSize / 2) (gridSize / 2)
    -- Energizer
    drawItem (Collectible Available Energizer) x y = translate x y 
        $ dotColor 
        $ circleSolid (gridSize / 2)
    -- Fruit
    drawItem (Collectible Available Fruit) x y = translate x y 
        $ color red 
        $ circleSolid (gridSize / 2)

-- Parse to int list to an grid
buildGrid :: [[Int]] -> Grid
buildGrid = map buildRow
    where 
        buildRow :: [Int] -> [GridItem]
        buildRow []      = []
        buildRow (0:xs)  = Empty : (buildRow xs)
        buildRow (1:xs)  = Wall : (buildRow xs)
        buildRow (2:xs)  = SpawnPoint : (buildRow xs)
        buildRow (3:xs)  = Door : (buildRow xs) 

-- Parse the grid to a list of pictures with coordinates for an offset       
showGrid :: Grid -> Float -> Float -> [Picture]
showGrid [] _ _ = []
showGrid (gis:grid) x y = (showRow gis x y) ++ (showGrid grid x (y - gridSize))
    where
        showRow :: [GridItem] -> Float -> Float -> [Picture]
        showRow [] _ _ = []
        showRow (g:gis) x y = (drawItem g x y) : (showRow gis (x + gridSize) y)