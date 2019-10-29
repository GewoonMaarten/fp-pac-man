module Models.Ghost where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Utils.Path

data GhostType = Blinky | Inky | Pinky | Clyde

data Ghost = Ghost {
    unPath :: Path,
    unDistance :: NodeDistance,
    unType :: GhostType
}

initialGhost t = Ghost [Pn 9 7] 1 t

showGhost (Ghost pns d t) = translate x (-y) $ c t $ circleSolid 5
  where 
    (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pns d))
    c Blinky = color red
    c Inky   = color blue
    c Pinky  = color pink
    c Clyde  = color orange
    pink = makeColorI 255 192 203 0
