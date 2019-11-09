--------------------------------------------------------------------------------
-- Utilities for the play scene
--------------------------------------------------------------------------------
module Utils.Play where

import           Config
import           Model
import           Models.Ghost
import           Models.PacMan
import           Models.Level

initialPlay :: [[Int]] -> GameState -> GameState
initialPlay level gameState = gameState
  { unScene  = Play
  , unLevel  = buildGrid level
  , unGhosts = [ initialGhost Blinky
               , initialGhost Inky
               , initialGhost Pinky
               , initialGhost Clyde
               ]
  , unPacMan = initialPacMan
  }

resetPlay :: GameState -> GameState
resetPlay gameState = gameState
  { unGhosts = [ initialGhost Blinky
               , initialGhost Inky
               , initialGhost Pinky
               , initialGhost Clyde
               ]
  }
