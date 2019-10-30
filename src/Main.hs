module Main where

import           Model
import           Models.PacMan
import           Models.Ghost
import           View
import           Controllers.KeyController
import           Collider
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import Debug.Trace
main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 700) (10, 10)) -- Display mode
              black -- Background Color
              30 -- Number of steps per second
              intialGameState -- Initial world
              draw -- (world -> IO Picture)
              input -- (Event -> world -> IO world)
              update -- (Float -> world -> IO world)

intialGameState :: GameState
intialGameState = GameState
    Home
    (Grid [] 0 0 0)
    initialPacMan
    [ initialGhost Blinky
    , initialGhost Inky
    , initialGhost Pinky
    , initialGhost Clyde
    ]

draw :: GameState -> IO Picture
draw = return . drawView

input :: Event -> GameState -> IO GameState
input event gameState = return (inputHandler event gameState)

update :: Float -> GameState -> IO GameState
update dt = return . performUpdate dt

performUpdate :: Float -> GameState -> GameState
performUpdate dt gs = collectItems $ gs
    { unPacMan = performPacManUpdate dt (unPacMan gs)
    , unGhosts = map (performGhostUpdate gs dt) (unGhosts gs)
    }
