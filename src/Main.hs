module Main where

import           Model
import           Models.PacMan
import           Models.Ghost
import           View
import           Controllers.KeyController
import           Utils.Collectible
import           Utils.Path
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Graphics
import           Utils.Animator

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
draw gs = drawView gs <$> loadTextures

input :: Event -> GameState -> IO GameState
input event gameState = return (inputHandler event gameState)

update :: Float -> GameState -> IO GameState
update dt = return . performUpdate dt

performUpdate :: Float -> GameState -> GameState
performUpdate dt gs = updateEnergizerTimers dt $ collectItems $ gs
    { unPacMan = updateAnimation dt $ performPacManUpdate dt (unPacMan gs)
    , unGhosts = map
                     (updateAnimation dt . performGhostUpdate (ghostFn gs) dt)
                     (unGhosts gs)
    }

-- implement ghost choice using GameState at this level
ghostFn gs p@(P (a : b : _) _) = P p $ newDistance p
  where
    p = b : map (uncurry Pn) (follow (canPass . get) step $ f b)
    f (Pn x y) = (x, y)
    d        = directions (canPass . get) b a
    (nx, ny) = head d
    step (x, y) = (x + nx, y + ny)

    get (x, y) = level !! y !! x
    level = getGridItems $ unLevel gs
