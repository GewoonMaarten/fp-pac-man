module Main where

import           Model
import           Models.PacMan
import           Models.Ghost
import           Controllers.Animator
import           Controllers.Input              ( inputHandler )
import           Controllers.Draw               ( drawScene )
import           Controllers.Update             ( updateScene )
import           Utils.Collectible
import           Utils.Path
import           Utils.Graphics

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
intialGameState = GameState Home (Grid [] 0 0 0) initialPacMan []

draw :: GameState -> IO Picture
draw gs = do
    textures <- loadTextures
    return (drawScene (unScene gs) textures gs)

input :: Event -> GameState -> IO GameState
input event gs = return (inputHandler (unScene gs) event gs)

update :: Float -> GameState -> IO GameState
update dt gs = return (updateScene (unScene gs) dt gs)
