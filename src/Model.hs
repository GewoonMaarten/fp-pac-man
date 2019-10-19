module Model where

data GameState = GameState {
    Scene :: Scene
    Level :: Grid
    Player :: Player
    Ghosts :: Ghosts
    Collectibles :: [Collectible]
}

type Scene = Level | Home | GameOver

data Postition = Postition (Float,Float)
data Orientation = Up | Right | Down | Left
data Transformation = Transformation Postition Orientation

-- TODO: move to player file
type Lives = Int
type Score = Int
data Player = Player Transformation Lives Score

-- TODO: move to seperate file
type CollectibleType = PacDot | Energizer | Fruit
type CollectibleState = Collected | Availible
type Collectible = Collectible CollectibleState CollectibleType

-- TODO: move to Grid file
type GridItem = Empty 
              | Door Orientation
              | Wall 
              | SpawnPoint 
              | Collectible

type Grid = [[GridItem]]

-- TODO: move to Ghost file
type GhostType = Blinky | Inky | Pinky | Clyde
type GhostState = Dead | Edible | Alive
data Ghost = Ghost Transformation GhostType GhostState

type Ghosts = (Ghost, Ghost, Ghost, Ghost)

-- Initial GameState
initState :: GameState
initState = GameState 
    Level 
    [[]] 
    (Player (Transformation (Postion (0,0)) Right) 3 0)
    ((Ghost (Transformation (Postion (0,0)) Right) Blinky Alive),
     (Ghost (Transformation (Postion (0,0)) Right) Blinky Alive),
     (Ghost (Transformation (Postion (0,0)) Right) Blinky Alive),
     (Ghost (Transformation (Postion (0,0)) Right) Blinky Alive))
    []