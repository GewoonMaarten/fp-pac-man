module Model where

data GameState = GameState {
    unScene :: Scene,
    unLevel :: Grid,
    unPacMan :: PacMan
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

data PathNode = Pn Int Int
type Path = [PathNode]
type NodeDistance = Float

data PacMan = PacMan {
    unPath :: Path,
    unDistance :: NodeDistance
}
