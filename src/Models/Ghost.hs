module Models.Ghost where

import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Graphics
import           Data.List                      ( find )

data GhostType = Blinky | Inky | Pinky | Clyde deriving (Eq, Show, Enum)
data GhostState = Normal GhostAnimStage | Edible GhostAnimStage | Dead deriving (Show)
data GhostAnimStage = GOne | GTwo
  deriving (Show, Eq, Enum, Bounded)

data Ghost = Ghost {
    unPathG :: Path,
    unType :: GhostType,
    unGhostState :: GhostState,
    unGhostAnimTimer :: Float,
    unGhostEdibleTimer :: Float
} deriving (Show)

initialGhost Blinky = Ghost (P [(6, 7), (12, 7)] 6) Blinky (Normal GOne) 0 0
initialGhost Inky   = Ghost (P [(6, 11), (6, 7)] 4) Inky (Normal GOne) 0 0
initialGhost Pinky  = Ghost (P [(12, 7), (12, 11)] 4) Pinky (Normal GOne) 0 0
initialGhost Clyde  = Ghost (P [(12, 11), (6, 11)] 6) Clyde (Normal GOne) 0 0

performGhostUpdate canPass pmPath dt g = g
  { unPathG = movePath' (ghostFn canPass pickFn) (dt * 4) (unPathG g)
  }
 where
  pickFn = pick $ unType g
  -- implement different pick behaviours depending on pmPath
  pick :: GhostType -> PathNode -> [PathDirection] -> PathDirection
  pick Blinky l ds = bestDirection l ds (source pmPath)
  pick Inky   l ds = bestDirection l ds (source pmPath)
  pick Pinky  l ds = bestDirection l ds (destination pmPath)
  pick Clyde  l ds = bestDirection l ds (destination pmPath)

getPos :: Path -> (Float, Float)
getPos p = ((-180, -255) Pt.+ 20 Pt.* (actualLocation p))

drawGhost :: Ghost -> Textures -> Picture
drawGhost g@(Ghost p gt (Normal stage) _ _) ts =
  case find (\v -> fromEnum gt == fst v) (texturesGhost ts) of
    Just a  -> translate x (-y) $ scale s s (getTexture stage (snd a))
    Nothing -> blank
 where
  getTexture GOne (GhostTextureSet p1 _ ) = p1
  getTexture GTwo (GhostTextureSet _  p1) = p1
  s      = 0.6
  (x, y) = getPos p
drawGhost g@(Ghost p _ (Edible stage) _ _) ts = translate x (-y)
  $ scale s s (getTexture stage (texturesGhostAfraid ts))
 where
  (x, y) = getPos p
  s      = 0.6
  getTexture GOne (GhostTextureSet p1 _ ) = p1
  getTexture GTwo (GhostTextureSet _  p1) = p1
