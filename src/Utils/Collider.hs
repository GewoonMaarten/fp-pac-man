module Utils.Collider
  ( collectItems
  )
where

import           Graphics.Gloss          hiding ( Path )
import           Models.Ghost            hiding ( unPath )
import           Models.PacMan
import           Model
import           Utils.Path

collectItems :: GameState -> GameState
collectItems g@(GameState{unLevel = Grid {getGridItems = []}}) = g
collectItems gs =
  let p                   = unPacMan gs
      score               = unScore p
      (px, py)            = actualLocation $ unPath p
      grid                = unLevel gs
      gridItems           = getGridItems grid
      (giss1, gi : giss2) = splitAt (floor py) gridItems
      (gis1 , g : gis2  ) = splitAt (floor px) gi
      (g1,s)              = pickup g  
      gis                 = gis1 ++ g1 : gis2
      giss                = giss1 ++ gis : giss2
  in  gs { unLevel = grid { getGridItems = giss }, unPacMan = p { unScore = score + s } }
 where
  pickup :: GridItem -> (GridItem,Int)
  pickup (Collectible Available t p) = ((Collectible Collected t p), p)
  pickup g                           = (g, 0)
