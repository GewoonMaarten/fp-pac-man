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

initialGhost Blinky = Ghost (P [Pn  6  7, Pn 12  7] 6) Blinky
initialGhost Inky   = Ghost (P [Pn  6 11, Pn  6  7] 4) Inky
initialGhost Pinky  = Ghost (P [Pn 12  7, Pn 12 11] 4) Pinky
initialGhost Clyde  = Ghost (P [Pn 12 11, Pn  6 11] 6) Clyde

performGhostUpdate gs dt g = g {
  unPath = movePath' (ghostFn gs) (dt * 4) (unPath g)
}

showGhost (Ghost p t) = translate x (-y) $ c t $ circleSolid 5
  where 
    (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation p))
    c Blinky = color red
    c Inky   = color blue
    c Pinky  = color pink
    c Clyde  = color orange
    pink = makeColorI 255 192 203 255
