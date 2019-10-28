module Controllers.KeyController
  ( inputHandler
  )
where

import           Model
import           Controllers.Level
import           Graphics.Gloss.Interface.IO.Game

inputHandler :: Event -> GameState -> GameState
-- Enter Key
inputHandler (EventKey (SpecialKey KeyEnter) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState
    Home     -> gameState { unScene = Play, unLevel = (buildGrid initGrid) }
    Pause    -> gameState { unScene = Home }
    GameOver -> gameState { unScene = Home }
-- Esc Key
inputHandler (EventKey (SpecialKey KeyEsc) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState { unScene = Pause }
    Pause    -> gameState { unScene = Play }
    Home     -> gameState
    GameOver -> gameState
-- Left Key
inputHandler (EventKey (SpecialKey KeyLeft) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState { unPacMan = move (unLevel gameState) (unPacMan gameState) }
    Pause    -> gameState
    Home     -> gameState
    GameOver -> gameState
    where
      move level pm@(PacMan [Pn x y] p) = pm { 
        unPath = [Pn x y, Pn nx y], 
        unDistance = abs $ fromIntegral (x - nx) 
      }
        where nx = nextX level x y (\t -> t - 1)
      move _     pm = pm

-- Right Key
inputHandler (EventKey (SpecialKey KeyRight) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState { unPacMan = move (unLevel gameState) (unPacMan gameState) }
    Pause    -> gameState
    Home     -> gameState
    GameOver -> gameState
    where
      move level pm@(PacMan [Pn x y] p) = pm { 
        unPath = [Pn x y, Pn nx y], 
        unDistance = abs $ fromIntegral (x - nx) 
      }
        where nx = nextX level x y (+1)
      move _     pm = pm
          
-- Up Key
inputHandler (EventKey (SpecialKey KeyUp) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState { unPacMan = move (unLevel gameState) (unPacMan gameState) }
    Pause    -> gameState
    Home     -> gameState
    GameOver -> gameState
    where
      move level pm@(PacMan [Pn x y] p) = pm { 
        unPath = [Pn x y, Pn x ny], 
        unDistance = abs $ fromIntegral (y - ny) 
      }
        where ny = nextY level x y (\t -> t - 1)
      move _     pm = pm
-- Down Key
inputHandler (EventKey (SpecialKey KeyDown) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState { unPacMan = move (unLevel gameState) (unPacMan gameState) }
    Pause    -> gameState
    Home     -> gameState
    GameOver -> gameState
    where
      move level pm@(PacMan [Pn x y] p) = pm { 
        unPath = [Pn x y, Pn x ny], 
        unDistance = abs $ fromIntegral (y - ny) 
      }
        where ny = nextY level x y (+1)
      move _     pm = pm

inputHandler _ gameState = gameState

nextX level x y step =
  case level !! y !! (step x) of
    Empty           -> nextX level (step x) y step
    Collectible _ _ -> nextX level (step x) y step
    _ -> x

nextY level x y step =
  case level !! (step y) !! x of
    Empty           -> nextY level x (step y) step
    Collectible _ _ -> nextY level x (step y) step
    _ -> y
