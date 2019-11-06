module Controllers.UpdateController where

import           Model
import           Models.PacMan
import           Models.Ghost
import           Controllers.Animator
import           Utils.Collectible
import           Utils.Path
import {-# SOURCE #-} Controllers.SceneController
                                                ( getScene )
updatePlay :: Float -> GameState -> GameState
updatePlay dt gs = checkGameOver $ updateEnergizerTimers dt $ collectItems $ gs
  { unPacMan = updateAnimation dt $ performPacManUpdate dt (unPacMan gs)
  , unGhosts = map gu $ unGhosts gs
  }
 where
  get = getGridItem $ unLevel gs
  pm  = performPacManUpdate dt (unPacMan gs)
  gu  = updateAnimation dt . performGhostUpdate (canPass . get) (unPath pm) dt
  checkGameOver gs1
    | null (getAvailableCollectibles gs1) = gs1 { unScene = getScene GameOver }
    | unLives (unPacMan gs1) == 0         = gs1 { unScene = getScene GameOver }
    | otherwise                           = gs1

updateBase _ gs = gs
