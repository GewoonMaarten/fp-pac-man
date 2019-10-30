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

type Position = (Float, Float)
type Radius = Float
type BoxSize = (Float, Float)

data CollectibleType = PacDot BoxSize
                     | Energizer Radius
                     | Fruit Radius
  deriving (Show)

data CollectibleState = Collected
                      | Available
  deriving (Show)

data GridItem = Empty BoxSize Position
              | Door BoxSize Position
              | Wall BoxSize Position
              | SpawnPoint BoxSize Position
              | Collectible Position CollectibleState CollectibleType
  deriving (Show)
