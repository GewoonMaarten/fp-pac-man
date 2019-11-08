--------------------------------------------------------------------------------
-- Utilities for the play scene
--------------------------------------------------------------------------------
module Utils.Play where

import           Config
import           Model
import           Models.Ghost
import           Models.PacMan
import           Models.Level

initialPlay :: GameState -> GameState
initialPlay gameState = gameState
  { unScene  = Play
  , unLevel  = buildGrid gridX gridY gridSize
  , unGhosts = [ initialGhost Blinky
               , initialGhost Inky
               , initialGhost Pinky
               , initialGhost Clyde
               ]
  }

resetPlay :: GameState -> GameState
resetPlay gameState = gameState
  { unGhosts = [ initialGhost Blinky
               , initialGhost Inky
               , initialGhost Pinky
               , initialGhost Clyde
               ]
  }
