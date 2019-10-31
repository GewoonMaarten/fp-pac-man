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

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight 
  deriving (Show, Enum)
data AnimStage = One | Two | Three | Four 
  deriving (Show, Eq, Enum, Bounded)

initialPacMan :: PacMan
initialPacMan = PacMan (P [Pn 9 7] 1) 0 (Just (Movement MoveLeft Three))

performPacManUpdate :: Float -> PacMan -> PacMan
performPacManUpdate dt pm = pm { unPath = movePath (dt * 4) (unPath pm) }

showPacMan :: TextureSet -> PacMan -> Picture
showPacMan ts pm@(PacMan _ _ (Just (Movement dir stage))) = let (x, y) = getLoc pm in 
  translate x (-y) $ pacmManRotate dir $ pacManScale $ getTexture ts stage
  where
    getTexture :: TextureSet -> AnimStage -> Picture
    getTexture ( PacManTextureSet p _ _ _ ) One   = p
    getTexture ( PacManTextureSet _ p _ _ ) Two   = p
    getTexture ( PacManTextureSet _ _ p _ ) Three = p
    getTexture ( PacManTextureSet _ _ _ p ) Four  = p
    getLoc :: PacMan -> (Float, Float)
    getLoc pm = ((-180, -255) Pt.+ 20 Pt.* (actualLocation $ unPath pm))
    pacManScale :: Picture -> Picture
    pacManScale = let s = 0.6 in scale s s
    pacmManRotate :: Direction -> Picture -> Picture
    pacmManRotate MoveUp    = rotate (-90)
    pacmManRotate MoveDown  = rotate 90
    pacmManRotate MoveLeft  = rotate 180
    pacmManRotate MoveRight = rotate 0

showScore :: PacMan -> Picture
showScore (PacMan _ s _) =
  translate 100 300 $ scale 0.2 0.2 $ color white $ text $ show s
