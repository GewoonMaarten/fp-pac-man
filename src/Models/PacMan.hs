module Models.PacMan where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Utils.Path

data PacMan = PacMan {
    unPath :: Path,
    unDistance :: NodeDistance
}

initialPacMan :: PacMan
-- initialPacMan = PacMan [Pn 0 0, Pn 100 0, Pn 100 100, Pn 0 100, Pn 0 0] 1
initialPacMan = PacMan [Pn 9 7] 1

performPacManUpdate :: Float -> PacMan -> PacMan
performPacManUpdate dt = move (dt * 4)
    where
        move _ pm@(PacMan [_] _) = pm
        move dt pm
            | nd <= 0   = move (-nd) $ next pm
            | otherwise = pm { unDistance = nd }
            where nd = (unDistance pm) - dt
        next (PacMan (_:npns) _) = PacMan npns $ dif npns
        dif [_] = 0
        dif ((Pn xa ya):(Pn xb yb):_) = fromIntegral $ abs $ (xb - xa) + (yb - ya)

showPacMan (PacMan pns d) = translate x (-y) $ color yellow $ circleSolid 10
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pns d))
