module Main where

import Level

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 500) (10, 10)) -- Display mode
       black -- Background Color
       30 -- Number of steps per second
       intialGameState -- Initial world
       draw -- (world -> IO Picture)
       input -- (Event -> world -> IO world)
       update -- (Float -> world -> IO world)


data GameState = GameState {
    unLevel :: Grid
}               

intialGameState :: GameState
intialGameState = GameState (buildGrid initGrid)


draw :: GameState -> IO Picture
draw = return . drawPure


drawPure :: GameState -> Picture
drawPure g = pictures (showGrid (unLevel g) (-200) 250)

input :: Event -> GameState -> IO GameState
input _ gstate = return gstate


update :: Float -> GameState -> IO GameState
update _ gstate = return gstate