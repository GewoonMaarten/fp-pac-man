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
updateScene Play dt = checkGameOver 
                    . updateEnergizerTimers dt 
                    . collectItems 
                    . collide 
                    . ghostUpdates 
                    . pacManUpdate
 where
  pacManUpdate :: GameState -> GameState
  pacManUpdate gs = gs { unPacMan = (updateAnimation dt . updatePacMan dt) (unPacMan gs) }
  -- To access random we need to fold over state
  -- because ghosts don't access eachother we can reset the list of ghosts and the fill it with foldr
  ghostUpdates gs = foldr ghostUpdate gs { unGhosts = [] } $ unGhosts gs
  ghostUpdate :: Ghost -> GameState -> GameState
  ghostUpdate g gs = addGhost (gu g) gs
    where
      addGhost :: Ghost -> GameState -> GameState
      addGhost g' gs' = gs' { unGhosts = g' : unGhosts gs' }
      gu :: Ghost -> Ghost
      gu = updateAnimation dt . performGhostUpdate (canPass . get) (unPath $ unPacMan gs) dt
      get = getGridItem $ unLevel gs
  
  checkGameOver :: GameState -> GameState
  checkGameOver gs1
    | null (getAvailableCollectibles gs1) = gs1 { unScene = GameOver }
    | unLives (unPacMan gs1) == 0         = gs1 { unScene = GameOver }
    | otherwise                           = gs1

  collide :: GameState -> GameState
  collide gs = f $ checkTouch (unPacMan gs) (unGhosts gs)
    where 
      f Nothing = gs
      f (Just (Ghost _ t (Edible _) _ _ _)) = (resetGhost t . addScore 100) $ gs
      f _ = resetGhosts $ gs { unPacMan = updateLives (unPacMan gs) }
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
updateScene _ _ = id
