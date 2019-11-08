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
import           Data.List                      ( find )
updateScene :: Scene -> Float -> GameState -> GameState
updateScene Play dt gs =
  checkGameOver $ case checkTouch (unPacMan gs) (unGhosts gs) of
    Just (Ghost _ _ (Edible _) _ _ _) ->
      gs { unGhosts = map (initialGhost . unType) (unGhosts gs) }
    Just _ -> gs { unPacMan = updateLives (unPacMan gs) (unGhosts gs)
                 , unGhosts = map (initialGhost . unType) (unGhosts gs)
                 }
    Nothing -> updateEnergizerTimers dt $ collectItems $ gs
      { unPacMan = updateAnimation dt $ updatePacMan dt (unPacMan gs)
      , unGhosts = map gu $ unGhosts gs
      }
 where
  get = getGridItem $ unLevel gs
  pm  = updatePacMan dt (unPacMan gs)
  gu  = updateAnimation dt . performGhostUpdate (canPass . get) (unPath pm) dt
  checkGameOver gs1
    | null (getAvailableCollectibles gs1) = gs1 { unScene = GameOver }
    | unLives (unPacMan gs1) == 0         = gs1 { unScene = GameOver }
    | otherwise                           = gs1
  checkTouch :: PacMan -> [Ghost] -> Maybe Ghost
  checkTouch pm gs =
    let
      (px, py) = actualLocation (unPath pm)
      ghostPos = actualLocation . unPathG
    in
      find
        (\g ->
          let (gx, gy) = ghostPos g
          in  floor gx == floor px && floor gy == floor py
        )
        gs
updateScene _ _ gs = gs




