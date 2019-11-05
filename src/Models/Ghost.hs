module Models.Ghost where

import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Graphics

data GhostType = Blinky | Inky | Pinky | Clyde deriving (Eq, Show, Enum)
data GhostState = Normal | Edible GhostAnimStage | Dead deriving (Show)
data GhostAnimStage = GOne | GTwo
  deriving (Show, Eq, Enum, Bounded)

data Ghost = Ghost {
    unPathG :: Path,
    unType :: GhostType,
    unGhostState :: GhostState,
    unGhostAnimTimer :: Float,
    unGhostEdibleTimer :: Float
} deriving (Show)

initialGhost Blinky = Ghost (P [Pn 6 7, Pn 12 7] 6) Blinky Normal 0 0
initialGhost Inky   = Ghost (P [Pn 6 11, Pn 6 7] 4) Inky Normal 0 0
initialGhost Pinky  = Ghost (P [Pn 12 7, Pn 12 11] 4) Pinky Normal 0 0
initialGhost Clyde  = Ghost (P [Pn 12 11, Pn 6 11] 6) Clyde Normal 0 0

performGhostUpdate ghostFn dt g =
  g { unPathG = movePath' ghostFn (dt * 4) (unPathG g) }

drawGhost :: Ghost -> [(Int, TextureSet)] -> Picture
drawGhost _ [] = blank
drawGhost g@(Ghost p gt _ _ _) (t : ts) | fromEnum gt == fst t = translate x (-y) $ scale s s p1
                                        | otherwise = drawGhost g ts
  where
    (GhostTextureSet p1 _) = snd t
    s                      = 0.6
    (x,y) =  ((-180, -255) Pt.+ 20 Pt.* (actualLocation p))