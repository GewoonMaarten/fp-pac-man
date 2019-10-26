module Model where

data GameState = GameState {
    unScene :: Scene,
    unLevel :: Grid
}

data Scene = Play | Pause | Home | GameOver

type Grid = [[GridItem]]

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
