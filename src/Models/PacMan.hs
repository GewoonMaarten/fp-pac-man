module Models.PacMan where

import           Models.Ghost
import           Config
import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Utils.Graphics
import           Data.Fixed                     ( mod' )
data PacMan = PacMan {
    unPath :: Path,
    unScore :: Int,
    unLives :: Int,
    unMovement :: Maybe Movement,
    unAnimTimer :: Float,
    unInvincibleTimer :: Float
} deriving (Show)

data Movement = Movement {
  unDirection :: Direction,
  unAnimStage :: AnimStage
} deriving (Show)

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Show, Enum)
data AnimStage = POne | PTwo | PThree | PFour
  deriving (Show, Eq, Enum, Bounded)

spawnPoint :: Path
spawnPoint = P [(9, 15)] 1

initialPacMan :: PacMan
initialPacMan = PacMan spawnPoint -- Path
                       0 -- Score
                       3 -- Lifes
                       (Just (Movement MoveLeft PThree)) -- Movement
                       0 -- Animation timer
                       0 -- Invincible Timer

updatePacMan :: Float -> PacMan -> PacMan
updatePacMan = updateMovement

updateMovement :: Float -> PacMan -> PacMan
updateMovement dt pm = pm
  { unPath            = path
  , unMovement        = mv (unMovement pm)
  , unInvincibleTimer = updateInvincibleTimer dt (unInvincibleTimer pm)
  }
 where
  path :: Path
  path = movePath (dt * 4) (unPath pm)
  mv :: Maybe Movement -> Maybe Movement
  mv (Just movement) =
    Just $ movement { unDirection = upd path $ unDirection movement }
  mv Nothing = Nothing
  upd :: Path -> Direction -> Direction
  upd (P [_        ] _) d = d
  upd (P (a : b : _) _) d = dir d a b
  dir :: (Eq a, Ord a) => Direction -> (a, a) -> (a, a) -> Direction
  dir d (x1, y1) (x2, y2) | x1 < x2   = MoveRight
                          | x1 > x2   = MoveLeft
                          | y1 < y2   = MoveDown
                          | y1 > y2   = MoveUp
                          | otherwise = d

updateInvincibleTimer :: Float -> Float -> Float
updateInvincibleTimer dt secs | secs - dt <= 0 = 0
                              | otherwise      = secs - dt

updateLives :: PacMan -> PacMan
updateLives pm =
  let lives = unLives pm
  in  pm { unLives = lives - 1, unInvincibleTimer = 5, unPath = spawnPoint }

showPacMan :: TextureSet -> PacMan -> Picture
showPacMan ts pm = let (x, y)   = getLoc pm
                       movement = unMovement pm
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
  getLoc pm = (gridX, -gridY) Pt.+ gridSize Pt.* actualLocation (unPath pm)
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
