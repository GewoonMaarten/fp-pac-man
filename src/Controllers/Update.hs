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
  checkGameOver $ updateLives $ updateEnergizerTimers dt $ collectItems $ gs
    { unPacMan = updateAnimation dt $ performPacManUpdate dt (unPacMan gs)
    , unGhosts = map gu $ unGhosts gs
    }
 where
  get = getGridItem $ unLevel gs
  pm  = performPacManUpdate dt (unPacMan gs)
  gu  = updateAnimation dt . performGhostUpdate (canPass . get) (unPath pm) dt
  checkGameOver gs1
    | null (getAvailableCollectibles gs1) = gs1 { unScene = GameOver }
    | unLives (unPacMan gs1) == 0         = gs1 { unScene = GameOver }
    | otherwise                           = gs1
  -- TODO: move this somewhere else
  updateLives :: GameState -> GameState
  updateLives gameState =
    let p  = unPacMan gameState
        pl = unLives p
        gs = unGhosts gameState
    in  gameState
          { unPacMan = p { unLives = if checkTouch p gs then pl - 1 else pl }
          }
   where
    checkTouch :: PacMan -> [Ghost] -> Bool
    checkTouch pm gs =
      let (px, py)  = actualLocation (unPath pm)
          ghostsPos = map (actualLocation . unPathG) gs
      in  any (\(gx, gy) -> floor gx == floor px && floor gy == floor py)
              ghostsPos

updateScene _ _ gs = gs
