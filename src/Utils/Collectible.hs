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
import           Debug.Trace

-- Number of seconds a ghost should be edible
ghostEdibleTimer = 10

collectItems :: GameState -> GameState
collectItems gs@GameState { unLevel = Grid { getGridItems = [] } } = gs
collectItems gs =
  let p                   = unPacMan gs
      score               = unScore p
      (px, py)            = actualLocation $ unPath p
      grid                = unLevel gs
      gridItems           = getGridItems grid
      (giss1, gi : giss2) = splitAt (floor py) gridItems
      (gis1 , g : gis2  ) = splitAt (floor px) gi
      (g1   , s         ) = pickup g
      gis                 = gis1 ++ g1 : gis2
      giss                = giss1 ++ gis : giss2
  in  doToggleEnergizer g $ gs { unLevel  = grid { getGridItems = giss }
                               , unPacMan = p { unScore = score + s }
                               }
 where
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
  checkEdible (Ghost _ _ (Edible _) _ _ _) = True
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
    | unIsReleased g = g
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
  updateEngerizerTimer g@(Ghost _ _ (Edible _) _ timer _)
    | timer - secs > 0 = g { unGhostEdibleTimer = timer - secs }
    | otherwise = g { unGhostEdibleTimer = 0, unGhostState = Normal GOne }
  updateEngerizerTimer g = g { unGhostEdibleTimer = 0 }
