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
    { unPacMan = performPacManUpdate dt (unPacMan gs)
    , unGhosts = map (performGhostUpdate (ghostFn gs) dt) (unGhosts gs)
    }

-- implement ghost choice using GameState at this level
ghostFn gs p@(P (a:b:_) _) = P p $ newDistance p
    where
        p = b : map (uncurry Pn) (follow $ f b)
        f (Pn x y) = (x, y)
        d = directions gs b (dir b a)
        (nx, ny) = head d
        step (x, y) = (x + nx, y + ny)
        follow p = f (wrap $ step p) (canPass $ get $ step p) 
            where
                -- wrap at bounds 
                f (Just wrapped) _    = p : wrapped : follow wrapped
                -- move forward
                f _              True = follow (step p)
                -- finish
                f _              _    = [p]
                
        get (x, y) = level !! y !! x
        level = getGridItems $ unLevel gs
    
dir (Pn x1 y1) (Pn x2 y2) = (d x1 x2, d y1 y2)
where 
    d a b
        | a < b     = 1
        | a > b     = -1
        | otherwise = 0

-- directions _  _        cameFrom = [cameFrom]
directions gs (Pn x y) cameFrom = filter canGoTo [(-1, 0), (1, 0), (0, -1), (0, 1)]
where 
canGoTo n@(nx, ny) = n /= cameFrom && (canPass $ get (x + nx, y + ny))

get (x, y) = level !! y !! x
level = getGridItems $ unLevel gs

canPass Empty               = True
canPass Collectible{}       = True
canPass _                   = False

-- wrap boundary nodes to the other side
wrap (-1, y) = Just (18, y)
wrap (19, y) = Just (0, y)
wrap (x, -1) = Just (x, 20)
wrap (x, 21) = Just (x, 0)
wrap _ = Nothing

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
