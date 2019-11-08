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
  checkGameOver $ case checkTouch pacMan ghosts of
    Just (Ghost _ _ (Edible _) _ _ _) -> addScore 100 $ gs { 
        unGhosts = map (initialGhost . unType) ghosts }
    Just _ -> gs { unPacMan = updateLives pacMan ghosts
                 , unGhosts = map (initialGhost . unType) ghosts
                 }
    Nothing -> (updateEnergizerTimers dt . collectItems . ghostUpdates . pacManUpdate) gs
 where
  pacMan = unPacMan gs
  ghosts = unGhosts gs
  score = unScore pacMan
  get = getGridItem $ unLevel gs
  pm  = updatePacMan dt pacMan

  pacManUpdate gs = gs { unPacMan = (updateAnimation dt . updatePacMan dt) (unPacMan gs) }
  -- To access random we need to fold over state
  -- because ghosts don't access eachother we can reset the list of ghosts and the fill it with foldr
  ghostUpdates gs = foldr ghostUpdate gs { unGhosts = [] } $ unGhosts gs
  ghostUpdate g gs = gs { unGhosts = (gu g) : unGhosts gs }
    where
      gu = updateAnimation dt . performGhostUpdate (canPass . get) (unPath pm) dt
      catch (Ghost _ _ (Edible _) _ _ _) = initialGhost $ unType g
      catch _                            = initialGhost $ unType g
      checkTouch = touches (actualLocation $ unPath $ unPacMan gs) (actualLocation $ unPathG g)
  
  checkGameOver gs1
    | null (getAvailableCollectibles gs1) = gs1 { unScene = GameOver }
    | unLives (unPacMan gs1) == 0         = gs1 { unScene = GameOver }
    | otherwise                           = gs1

  touches (px, py) (gx, gy) = floor gx == floor px && floor gy == floor py
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
