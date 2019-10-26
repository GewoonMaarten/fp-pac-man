module Main where

import           Model
import           View
import           Controllers.KeyController

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 700) (10, 10)) -- Display mode
              black -- Background Color
              30 -- Number of steps per second
              intialGameState -- Initial world
              draw -- (world -> IO Picture)
              input -- (Event -> world -> IO world)
              update -- (Float -> world -> IO world)

intialGameState :: GameState
intialGameState = GameState Home []

draw :: GameState -> IO Picture
draw = return . drawView

input :: Event -> GameState -> IO GameState
input event gameState = return (inputHandler event gameState)

update :: Float -> GameState -> IO GameState
update _ gstate = return gstate
