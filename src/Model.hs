module Model where

import           Models.PacMan
import           Models.Ghost
import           Utils.Graphics
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Random

data GameState = GameState {
    unScene :: Scene,
    unLevel :: Grid,
    unPacMan :: PacMan,
    unGhosts :: [Ghost],
    unRandom :: StdGen
}

addScore :: Int -> GameState -> GameState
addScore s gs = gs { unPacMan = addScoreM $ unPacMan gs }
  where addScoreM pm = pm { unScore = unScore pm + s }

resetGhosts :: GameState -> GameState
resetGhosts gs = gs { unGhosts = map (initialGhost . unType) $ unGhosts gs }

resetGhost :: GhostType -> GameState -> GameState
resetGhost t gs = gs { unGhosts = map r $ unGhosts gs }
  where r g = if unType g == t then initialGhost t else g

data Scene = Play | Pause | Home | GameOver
  deriving(Eq, Show)

data Grid = Grid {
  getGridItems :: [[GridItem]],
  getGridX :: Float,
  getGridY :: Float,
  getGridSize :: Float
} deriving (Show)


data CollectibleType = PacDot
                     | Energizer
                     | Fruit
  deriving (Eq, Show)

data CollectibleState = Collected
                      | Available
  deriving (Eq, Show)

type CollectibleScore = Int

data GridItem = Empty
              | Door
              | Wall
              | SpawnPoint
              | Collectible CollectibleState CollectibleType CollectibleScore
  deriving (Eq, Show)

canPass Empty         = True
canPass Collectible{} = True
canPass _             = False

getGridItem grid (x, y) = level !! y !! x where level = getGridItems grid
