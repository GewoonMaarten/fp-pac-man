module Model where

import Models.PacMan
import Models.Ghost

data GameState = GameState {
    unScene :: Scene,
    unLevel :: Grid,
    unPacMan :: PacMan,
    unGhosts :: [Ghost]
}

data Scene = Play | Pause | Home | GameOver
  deriving(Eq)

data Grid = Grid {
  getGridItems :: [[GridItem]],
  getGridX :: Float,
  getGridY :: Float,
  getGridSize :: Float
}

data CollectibleType = PacDot
                     | Energizer
                     | Fruit
  deriving (Show)

data CollectibleState = Collected
                      | Available
  deriving (Show)

data GridItem = Empty
              | Door
              | Wall
              | SpawnPoint
              | Collectible CollectibleState CollectibleType
  deriving (Show)
