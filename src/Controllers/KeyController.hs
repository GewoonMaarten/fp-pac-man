module Controllers.KeyController
  ( inputHandler
  )
where

import           Model
import           PacMan
import           Controllers.Level
import           Graphics.Gloss.Interface.IO.Game
import           Data.Fixed(mod')

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
inputHandler (EventKey (SpecialKey KeyLeft) _ _ _) gameState = scene Play (moveFn (\(x, y) -> (x - 1, y))) gameState
-- Right Key
inputHandler (EventKey (SpecialKey KeyRight) _ _ _) gameState = scene Play (moveFn (\(x, y) -> (x + 1, y))) gameState
-- Up Key
inputHandler (EventKey (SpecialKey KeyUp) _ _ _) gameState = scene Play (moveFn (\(x, y) -> (x, y - 1))) gameState
-- Down Key
inputHandler (EventKey (SpecialKey KeyDown) _ _ _) gameState = scene Play (moveFn (\(x, y) -> (x, y + 1))) gameState

inputHandler _ gameState = gameState

scene s fn gameState = if unScene gameState == s then fn gameState else gameState
moveFn dirFn gameState = gameState { unPacMan = move (unLevel gameState) dirFn (unPacMan gameState) }

move :: Grid -> ((Int, Int) -> (Int, Int)) -> PacMan -> PacMan
move level step pm@(PacMan [pn] _) = pm {
  unPath = [pn, npn],
  unDistance = ln pn npn
}
  where
    npn = nextPn pn
    nextPn (Pn x y) = (uncurry Pn) $ next (x, y)
    next p = if canPass $ get $ step p then next (step p) else p
    get (x, y) = level !! y !! x
    canPass Empty             = True
    canPass (Collectible _ _) = True
    canPass _                 = False
    ln (Pn x1 y1) (Pn x2 y2) = fromIntegral $ abs $ (x2 - x1) + (y2 - y1)
move level step pm@(PacMan (a:b:_) d) = if canPass $ get (step upN) then go else pm
  where 
    go = pm {
      unPath     = [a, (uncurry Pn) upN, (uncurry Pn) $ next (step upN)],
      unDistance = rd
    }
    upN      = upcomingNode a b d
    rd       = d `mod'` 1
    
    next p = if canPass $ get $ step p then next (step p) else p
    get (x, y) = level !! y !! x
    canPass Empty             = True
    canPass (Collectible _ _) = True
    canPass _                 = False
-- find direction
-- Find immedient coords
-- [a]
-- can move to step? -> change direction
-- move to next node using direction
-- recursive n times