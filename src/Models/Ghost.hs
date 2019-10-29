module Models.Ghost where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Utils.Path

data GhostType = Blinky | Inky | Pinky | Clyde

data Ghost = Ghost {
    unPath :: Path,
    unType :: GhostType
}

initialGhost t = Ghost (P [Pn 9 7] 1) t

performGhostUpdate :: Float -> Ghost -> Ghost
performGhostUpdate dt g = g {
  unPath = movePath (dt * 4) (unPath g)
}

showGhost (Ghost p t) = translate x (-y) $ c t $ circleSolid 5
  where 
    (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation p))
    c Blinky = color red
    c Inky   = color blue
    c Pinky  = color pink
    c Clyde  = color orange
    pink = makeColorI 255 192 203 0
