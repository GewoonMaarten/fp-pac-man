module Main where

import           Model
import           Models.PacMan
import           Models.Ghost
import           View
import           Controllers.KeyController
import           Utils.Collider
import           Utils.Path
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Graphics

main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 700) (10, 10)) -- Display mode
              black -- Background Color
              30 -- Number of steps per second
              intialGameState -- Initial world
              draw -- (world -> IO Picture)
              input -- (Event -> world -> IO world)
              update -- (Float -> world -> IO world)

intialGameState :: GameState
intialGameState = GameState
    0
    Home
    (Grid [] 0 0 0)
    initialPacMan
    []

draw :: GameState -> IO Picture
draw gs = drawView gs <$> loadTextures

input :: Event -> GameState -> IO GameState
input event gameState = return (inputHandler event gameState)

update :: Float -> GameState -> IO GameState
update dt = return . performUpdate dt

performUpdate :: Float -> GameState -> GameState
performUpdate dt gs = updateAnimation dt $ collectItems $ gs
    { unPacMan = pm
    , unGhosts = map gu $ unGhosts gs
    }
    where
        get = getGridItem $ unLevel gs
        pm  = performPacManUpdate dt (unPacMan gs)
        gu  = performGhostUpdate (canPass . get) (unPath pm) dt

-- number of seconds to wait to update the animation    
animationUpdateTime = 0.1

updateAnimation :: Float -> GameState -> GameState
updateAnimation secs gs@(GameState _ Play _ p@(PacMan _ _ _ (Just (Movement dir stage))) _)
    | elapsedTime gs + secs > animationUpdateTime
    = gs { elapsedTime = 0
         , unPacMan = p{unMovement = Just (Movement dir (nextStage stage))}
         }
    | otherwise
    = gs { elapsedTime = elapsedTime gs + secs }
  where
    nextStage :: AnimStage -> AnimStage
    nextStage s | fromEnum s + 1 > fromEnum (maxBound :: AnimStage) = toEnum 0
                | otherwise = succ s
updateAnimation secs gs = gs { elapsedTime = elapsedTime gs + secs }
