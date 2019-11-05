module Models.Ghost where

import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Graphics

data GhostType = Blinky | Inky | Pinky | Clyde deriving (Eq, Show, Enum)
type IsAfraid = Bool

data Ghost = Ghost {
    unPathG :: Path,
    unType :: GhostType,
    unIsAfraid :: IsAfraid
} deriving (Show)

initialGhost Blinky = Ghost (P [(6, 7), (12, 7)] 6) Blinky
initialGhost Inky   = Ghost (P [(6, 11), (6, 7)] 4) Inky
initialGhost Pinky  = Ghost (P [(12, 7), (12, 11)] 4) Pinky
initialGhost Clyde  = Ghost (P [(12, 11), (6, 11)] 6) Clyde

performGhostUpdate canPass pmPath dt g =
  g { unPathG = movePath' (ghostFn canPass pmPath) (dt * 4) (unPathG g) }

drawGhost :: Ghost -> [(Int, TextureSet)] -> Picture
drawGhost _ [] = blank
drawGhost g@(Ghost p gt False) (t : ts) | fromEnum gt == fst t = translate x (-y) $ scale s s p1
                                        | otherwise = drawGhost g ts
  where
    (GhostTextureSet p1 _) = snd t
    s                      = 0.6
    (x,y) =  ((-180, -255) Pt.+ 20 Pt.* (actualLocation p))