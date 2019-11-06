module Models.PacMan where

import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Utils.Graphics

data PacMan = PacMan {
    unPath :: Path,
    unScore :: Int,
    unLives :: Int,
    unMovement :: Maybe Movement,
    unAnimTimer :: Float
} deriving (Show)

data Movement = Movement {
  unDirection :: Direction,
  unAnimStage :: AnimStage
} deriving (Show)

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Show, Enum)
data AnimStage = POne | PTwo | PThree | PFour
  deriving (Show, Eq, Enum, Bounded)

initialPacMan :: PacMan
initialPacMan = PacMan (P [(9, 7)] 1) -- Path
                       0 -- Score
                       3 -- Lifes
                       (Just (Movement MoveLeft PThree)) -- Movement
                       0 -- Animation timer

performPacManUpdate :: Float -> PacMan -> PacMan
performPacManUpdate dt pm = pm { unPath     = path
                               , unMovement = mv (unMovement pm)
                               }
 where
  path = movePath (dt * 4) (unPath pm)
  mv (Just movement) =
    Just $ movement { unDirection = upd path $ unDirection movement }
  mv Nothing = Nothing
  upd (P [_        ] _) d = d
  upd (P (a : b : _) _) d = dir d a b
  dir d (x1, y1) (x2, y2) | x1 < x2   = MoveRight
                          | x1 > x2   = MoveLeft
                          | y1 < y2   = MoveDown
                          | y1 > y2   = MoveUp
                          | otherwise = d

showPacMan :: TextureSet -> PacMan -> Picture
showPacMan ts pm@(PacMan _ _ _ movement _) =
  let (x, y) = getLoc pm
  in  translate x (-y) $ case movement of
        Just (Movement dir stage) ->
          pacmManRotate dir $ pacManScale $ getTexture ts stage
        Nothing -> pacManScale $ getTexture ts PThree
 where
  getTexture :: TextureSet -> AnimStage -> Picture
  getTexture (PacManTextureSet p _ _ _) POne   = p
  getTexture (PacManTextureSet _ p _ _) PTwo   = p
  getTexture (PacManTextureSet _ _ p _) PThree = p
  getTexture (PacManTextureSet _ _ _ p) PFour  = p
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
showScore =
  translate 100 300 . scale 0.2 0.2 . color white . text . show . unScore

showLives :: Textures -> PacMan -> Picture
showLives t p =
  let x = (-200)
      y = (-180)
  in  pictures $ foldr translateTexture [translate x y blank] (lifeTextures t p)
 where
  lifeTextures :: Textures -> PacMan -> [Picture]
  lifeTextures ts pm =
    replicate (unLives pm) (scale 1.5 1.5 $ textureLifeCounter ts)
  translateTexture p ps@(Translate x1 y1 _ : _) = translate (x1 + 30) y1 p : ps
