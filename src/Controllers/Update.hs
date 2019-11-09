module Controllers.Update
  ( updateScene
  )
where

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
                    . wakeUpGhost
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

  wakeUpGhost :: GameState -> GameState
  wakeUpGhost gs = f awakeGhosts
    where
      f 1 = randomWakeUp 1 10
      f 2 = if (unScore . unPacMan) gs > 300 then randomWakeUp 2 10 else gs
      f 3 = if (unScore . unPacMan) gs > 800 then randomWakeUp 3 10 else gs
      f _ = gs
      randomWakeUp i chance = if r <= chance then wakeGhost i (setDestiniation [(9, 9), (9, 7)]) gs' else gs'
        where (r, gs') = getRandom 1000 gs
      awakeGhosts = length $ filter isAwake (unGhosts gs)

  collide :: GameState -> GameState
  collide gs = f $ checkTouch (unPacMan gs) (unGhosts gs)
    where 
      f Nothing = gs
      f (Just (Ghost _ t (Edible _) _ _)) = (resetGhost t . addScore 100) $ gs
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
              in  abs (gx - px) < 1 && abs (gy - py) < 1
            )
            gs
updateScene _ _ = id
