module Main where

import           Model
import           View
import           Controllers.KeyController

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pac-Man" (400, 700) (10, 10)) -- Display mode
              black -- Background Color
              30 -- Number of steps per second
              intialGameState -- Initial world
              draw -- (world -> IO Picture)
              input -- (Event -> world -> IO world)
              update -- (Float -> world -> IO world)

intialGameState :: GameState
intialGameState = GameState Home [] initialPacMan

initialPacMan :: PacMan
-- initialPacMan = PacMan [Pn 0 0, Pn 100 0, Pn 100 100, Pn 0 100, Pn 0 0] 1
initialPacMan = PacMan [Pn 9 7] 1

draw :: GameState -> IO Picture
draw = return . drawView

input :: Event -> GameState -> IO GameState
input event gameState = return (inputHandler event gameState)

update :: Float -> GameState -> IO GameState
update dt = return . performUpdate dt

performUpdate :: Float -> GameState -> GameState
performUpdate dt gs = gs { unPacMan = performPacManUpdate dt (unPacMan gs) }

performPacManUpdate :: Float -> PacMan -> PacMan
performPacManUpdate dt = move (dt * 4)
    where
        move _ pm@(PacMan [_] _) = pm
        move dt pm
            | nd <= 0   = move (-nd) $ next pm
            | otherwise = pm { unDistance = nd }
            where nd = (unDistance pm) - dt
        next (PacMan (_:npns) _) = PacMan npns $ dif npns
        dif [_] = 0
        dif ((Pn xa ya):(Pn xb yb):_) = fromIntegral $ abs $ (xb - xa) + (yb - ya)
