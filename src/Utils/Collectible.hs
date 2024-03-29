module Utils.Collectible
  ( collectItems
  , toggleEnergizer
  , updateEnergizerTimers
  , getAvailableCollectibles
  )
where

import           Graphics.Gloss          hiding ( Path )
import           Models.Ghost            hiding ( unPath )
import           Models.PacMan
import           Model
import           Utils.Path
import           Data.List

-- Number of seconds a ghost should be edible
ghostEdibleTimer = 10

collectItems :: GameState -> GameState
collectItems gs@GameState { unLevel = Grid { getGridItems = [] } } = gs
collectItems gs = foldr collect gs pmTiles
 where
  (px, py) = actualLocation $ unPath (unPacMan gs)
  floorceil a = nub [floor a, ceiling a]
  pmTiles = (,) <$> floorceil px <*> floorceil py

  collect :: PathNode -> GameState -> GameState
  collect l gs = (doToggleEnergizer g . addScore s) $ gs { unLevel = giss }
    where
      grid = unLevel gs
      g = getGridItem grid l
      (g1, s) = pickup g
      giss = replaceGridItem l g1 grid

  pickup :: GridItem -> (GridItem, Int)
  pickup (Collectible Available t p) = (Collectible Collected t p, p)
  pickup g                           = (g, 0)

  doToggleEnergizer :: GridItem -> GameState -> GameState
  doToggleEnergizer (Collectible Available Energizer _) gs
    | isToggled gs = gs
    | otherwise    = toggleEnergizer gs
  doToggleEnergizer _ gs = gs

getAvailableCollectibles :: GameState -> [GridItem]
getAvailableCollectibles gs =
  let level     = unLevel gs
      gridItems = getGridItems level
  in  filter
          (\gi -> case gi of
            Collectible Available _ _ -> True
            _                         -> False
          )
        $ concat gridItems

isToggled :: GameState -> Bool
isToggled gs = any checkEdible $ unGhosts gs
 where
  checkEdible (Ghost _ _ (Edible _) _ _) = True
  checkEdible _                            = False

toggleEnergizer :: GameState -> GameState
toggleEnergizer gs | isToggled gs = removeEnergizer gs
                   | otherwise    = applyEnergizer gs
 where
  applyEnergizer, removeEnergizer :: GameState -> GameState
  applyEnergizer  = changeGhostsState (Edible GOne)
  removeEnergizer = changeGhostsState (Normal GOne)
  changeGhostsState :: GhostState -> GameState -> GameState
  changeGhostsState _ gs@GameState { unGhosts = [] } = gs
  changeGhostsState gst gs =
    gs { unGhosts = map (changeGhostState gst) $ unGhosts gs }
  changeGhostState gst g
    | isAwake g = g
      { unGhostState       = gst
      , unGhostEdibleTimer = case gst of
                               Edible _ -> unGhostEdibleTimer g + 10
                               Normal _ -> 0
      }
    | otherwise = g
    
updateEnergizerTimers :: Float -> GameState -> GameState
updateEnergizerTimers secs gs =
  let ghosts = unGhosts gs in gs { unGhosts = map updateEngerizerTimer ghosts }
 where
  updateEngerizerTimer g@(Ghost _ _ (Edible _) _ timer)
    | timer - secs > 0 = g { unGhostEdibleTimer = timer - secs }
    | otherwise = g { unGhostEdibleTimer = 0, unGhostState = Normal GOne }
  updateEngerizerTimer g = g { unGhostEdibleTimer = 0 }
