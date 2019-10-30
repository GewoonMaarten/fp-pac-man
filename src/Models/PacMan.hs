module Models.PacMan where

import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Graphics

data PacMan = PacMan {
    unPath :: Path,
    unScore :: Int,
    unMovement :: Maybe Movement
} deriving (Show)

data Movement = Movement {
  unDirection :: Direction,
  unAnimStage :: AnimStage
} deriving (Show)

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Show, Enum, Bounded)
data AnimStage = One | Two | Three | Four deriving (Show, Eq, Enum, Bounded)

initialPacMan :: PacMan
initialPacMan = PacMan (P [Pn 9 7] 1) 0 (Just (Movement MoveLeft Three))

performPacManUpdate :: Float -> PacMan -> PacMan
performPacManUpdate dt pm = pm { unPath = movePath (dt * 4) (unPath pm) }

s = 0.6
-- ToDo add rotation
showPacMan :: TextureSet -> PacMan -> Picture
showPacMan (PacManTextureSet p _ _ _) (PacMan pm _ (Just (Movement dir One))) =
  translate x (-y) $ scale s s $ p
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pm))
showPacMan (PacManTextureSet _ p _ _) (PacMan pm _ (Just (Movement dir Two))) =
  translate x (-y) $ scale s s $ p
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pm))
showPacMan (PacManTextureSet _ _ p _) (PacMan pm _ (Just (Movement dir Three)))
  = translate x (-y) $ scale s s $ p
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pm))
showPacMan (PacManTextureSet _ _ _ p) (PacMan pm _ (Just (Movement dir Four)))
  = translate x (-y) $ scale s s $ p
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pm))

showScore (PacMan _ s _) =
  translate 100 300 $ scale 0.2 0.2 $ color white $ text $ show s
