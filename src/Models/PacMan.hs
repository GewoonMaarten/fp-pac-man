module Models.PacMan where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Utils.Path

data PacMan = PacMan {
    unPath :: Path
}

initialPacMan :: PacMan
initialPacMan = PacMan (P [Pn 9 7] 1)

performPacManUpdate :: Float -> PacMan -> PacMan
performPacManUpdate dt pm = pm {
  unPath = movePath (dt * 4) (unPath pm)
}

showPacMan (PacMan p) = translate x (-y) $ color yellow $ circleSolid 10
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation p))
