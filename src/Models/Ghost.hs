module Models.Ghost where

import           Config
import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Utils.Path
import           Utils.Graphics
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

initialGhost Blinky =
  Ghost (P [(9, 7), (12, 7)] 6) Blinky (Normal GOne) 0 0
initialGhost Inky  = Ghost (P [(9, 9)] 1) Inky (Normal GOne) 0 0
initialGhost Pinky = Ghost (P [(8, 9)] 1) Pinky (Normal GOne) 0 0
initialGhost Clyde = Ghost (P [(10, 9)] 1) Clyde (Normal GOne) 0 0

performGhostUpdate canPass pmPath dt g = g
  { unPathG = movePath' (ghostFn canPass pickFn) (dt * 4) (unPathG g)
  }
 where
  pickFn = pick $ unType g
  -- implement different pick behaviours depending on pmPath
  pick :: GhostType -> PathNode -> [PathDirection] -> PathDirection
  pick Blinky l ds = bestDirection l ds (destination pmPath)
  pick Inky   l ds = bestDirection l ds (source pmPath)
  pick Pinky  l ds = bestDirection l ds (destination pmPath)
  pick Clyde  l ds = bestDirection l ds (source pmPath)

getPos :: Path -> (Float, Float)
getPos p = (gridX, -gridY) Pt.+ gridSize Pt.* actualLocation p

drawGhost :: Ghost -> Textures -> Picture
drawGhost g ts = case unGhostState g of
  Normal stage -> n stage
  Edible stage -> a stage
 where
  (x, y) = getPos (unPathG g)
  s      = 0.6
  getTexture GOne (GhostTextureSet p1 _ ) = p1
  getTexture GTwo (GhostTextureSet _  p1) = p1
  t = unType g
  n stage = case find (\v -> fromEnum t == fst v) (texturesGhost ts) of
    Just a  -> translate x (-y) $ scale s s (getTexture stage (snd a))
    Nothing -> blank
  a stage =
    translate x (-y) $ scale s s (getTexture stage (texturesGhostAfraid ts))

isAwake :: Ghost -> Bool
isAwake = not . isStationary . unPathG
