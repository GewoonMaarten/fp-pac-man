module Controllers.Update
  ( updateScene
  )
where

import           Debug.Trace

import           Controllers.Animator
import           Model
import           Models.Ghost
import           Models.PacMan
import           Utils.Collectible
import           Utils.Path

updateScene :: Scene -> Float -> GameState -> GameState
updateScene Play dt gs =
  checkGameOver $ updateEnergizerTimers dt $ collectItems $ gs
    { unPacMan = updateAnimation dt
                   $ performPacManUpdate dt (unPacMan gs) (unGhosts gs)
    , unGhosts = map gu $ unGhosts gs
    }
 where
  get = getGridItem $ unLevel gs
  pm  = performPacManUpdate dt (unPacMan gs) (unGhosts gs)
  gu  = updateAnimation dt . performGhostUpdate (canPass . get) (unPath pm) dt
  checkGameOver gs1
    | null (getAvailableCollectibles gs1) = gs1 { unScene = GameOver }
    | unLives (unPacMan gs1) == 0         = gs1 { unScene = GameOver }
    | otherwise                           = gs1

updateScene _ _ gs = gs
