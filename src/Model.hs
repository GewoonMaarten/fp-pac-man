module Model where

import           Models.PacMan
import           Models.Ghost
import           Utils.Graphics
import           Graphics.Gloss          hiding ( Path )
import           System.Random
import           Utils.Path
import           Utils.ScoreBoard

data GameState = GameState {
    unScene :: Scene,
    unLevel :: Grid,
    unPacMan :: PacMan,
    unGhosts :: [Ghost],
    unRandom :: StdGen,
    unScores :: [Score],
    unDebug :: Bool
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
wakeGhost i f gs = gs { unGhosts = zipWith (curry r) [0 ..] (unGhosts gs) }
 where
  r (j, g) = if j == i then w g else g
  w :: Ghost -> Ghost
  w g = g { unPathG = f $ unPathG g }

getRandom :: Int -> GameState -> (Int, GameState)
getRandom i gs = fmap setRandom $ randomR (0, i) $ unRandom gs
  where setRandom r = gs { unRandom = r }

data Scene = Play | Pause | Home | GameOver String
  deriving(Eq, Show)

data Grid = Grid {
  getGridItems :: [[GridItem]],
  getGridX :: Float,
  getGridY :: Float,
  getGridSize :: Float
} deriving (Show)

asList :: [[GridItem]] -> [(PathNode, GridItem)]
asList gis = [((x, y), c) | (y, r) <- zip [0..] gis, (x, c) <- zip [0..] r]

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

isFruit (Collectible _ Fruit _) = True
isFruit _ = False

canPass Empty         = True
canPass Collectible{} = True
canPass _             = False

getGridItem grid (x, y) = level !! y !! x where level = getGridItems grid

replaceGridItem :: PathNode -> GridItem -> Grid -> Grid
replaceGridItem (x, y) g1 grid = grid { getGridItems = giss }
  where
    gridItems = getGridItems grid
    (giss1, gi : giss2) = splitAt y gridItems
    (gis1 , _ : gis2  ) = splitAt x gi
    gis                 = gis1 ++ g1 : gis2
    giss                = giss1 ++ gis : giss2

isCollected :: GridItem -> Bool
isCollected (Collectible Collected _ _) = True
isCollected _ = False
