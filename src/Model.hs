module Model where

import           Models.PacMan
import           Models.Ghost

data GameState = GameState {
    elapsedTime :: Float,
    unScene :: Scene,
    unLevel :: Grid,
    unPacMan :: PacMan,
    unGhosts :: [Ghost]
} deriving (Show)

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
  deriving (Show)

data CollectibleState = Collected
                      | Available
  deriving (Show)

type CollectibleScore = Int

data GridItem = Empty
              | Door
              | Wall
              | SpawnPoint
              | Collectible CollectibleState CollectibleType CollectibleScore
  deriving (Show)
