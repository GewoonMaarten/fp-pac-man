module Model where

import           Models.PacMan
import           Models.Ghost
import           Utils.Graphics
import           Graphics.Gloss          hiding ( Path )
import           System.Random
import           Utils.Path

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
  
wakeGhost :: Int -> (Path -> Path) -> GameState -> GameState
wakeGhost i f gs = gs { unGhosts = map r $ zip [0..] $ unGhosts gs }
  where 
    r (j, g) = if j == i then w g else g
    w :: Ghost -> Ghost
    w g = g { unPathG = f $ unPathG g }

getRandom :: Int -> GameState -> (Int, GameState)
getRandom i gs = fmap setRandom $ randomR (0, i) $ unRandom gs
  where setRandom r = gs { unRandom = r }

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
