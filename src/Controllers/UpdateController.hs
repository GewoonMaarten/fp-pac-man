module Controllers.UpdateController where

import           Model
import           Models.PacMan
import           Models.Ghost
import           Controllers.Animator
import           Utils.Collectible
import           Utils.Path
import           Graphics

performUpdate :: Float -> GameState -> GameState
performUpdate dt gs = updateEnergizerTimers dt $ collectItems $ gs
  { unPacMan = updateAnimation dt $ performPacManUpdate dt (unPacMan gs)
  , unGhosts = map gu $ unGhosts gs
  }
 where
  get = getGridItem $ unLevel gs
  pm  = performPacManUpdate dt (unPacMan gs)
  gu  = updateAnimation dt . performGhostUpdate (canPass . get) (unPath pm) dt

updateBase _ gs = gs
