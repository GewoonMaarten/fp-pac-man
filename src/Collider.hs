module Collider
  ( collectItems
  )
where

import           Graphics.Gloss          hiding ( Path )
import           Models.Ghost            hiding ( unPath )
import           Models.PacMan
import           Model
import           Utils.Path

collectItems :: GameState -> GameState
collectItems gs =
  let (px, py)            = actualLocation $ unPath $ unPacMan gs
      grid                = unLevel gs
      gridItems           = getGridItems grid
      (giss1, gi : giss2) = splitAt (floor py) gridItems
      (gis1 , g : gis2  ) = splitAt (floor px) gi
      gis                 = gis1 ++ (pickup g) : gis2
      giss                = giss1 ++ gis : giss2
  in  gs { unLevel = grid { getGridItems = giss } }
 where
  pickup :: GridItem -> GridItem
  pickup (Collectible Available t) = (Collectible Collected t)
  pickup g                         = g
