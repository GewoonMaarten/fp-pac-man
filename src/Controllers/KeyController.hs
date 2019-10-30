module Controllers.KeyController
  ( inputHandler
  )
where

import           Model
import           Models.PacMan
import           Utils.Path
import           Controllers.Level
import           Graphics.Gloss.Interface.IO.Game
import           Data.Fixed                     ( mod' )

inputHandler :: Event -> GameState -> GameState
-- Enter Key
inputHandler (EventKey (SpecialKey KeyEnter) _ _ _) gameState =
  case unScene gameState of
    Play     -> gameState
    Home     -> gameState { unScene = Play, unLevel = buildGrid (-180) 255 20 }
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
  scene Play (moveFn (\(x, y) -> (x - 1, y))) gameState
-- Right Key
inputHandler (EventKey (SpecialKey KeyRight) _ _ _) gameState =
  scene Play (moveFn (\(x, y) -> (x + 1, y))) gameState
-- Up Key
inputHandler (EventKey (SpecialKey KeyUp) _ _ _) gameState =
  scene Play (moveFn (\(x, y) -> (x, y - 1))) gameState
-- Down Key
inputHandler (EventKey (SpecialKey KeyDown) _ _ _) gameState =
  scene Play (moveFn (\(x, y) -> (x, y + 1))) gameState

inputHandler _ gameState = gameState

scene s fn gameState =
  if unScene gameState == s then fn gameState else gameState
moveFn dirFn gameState =
  gameState { unPacMan = move (unLevel gameState) dirFn (unPacMan gameState) }

move :: Grid -> ((Int, Int) -> (Int, Int)) -> PacMan -> PacMan
move grid step pm@(PacMan p _ _) = if upN /= finalN then mutate else pm
 where
  mutate = pm { unPath = P [head $ unNodes p, n upN, n finalN] rd }
  upN    = upcomingNode p
  finalN = final upN
  -- new target is upcoming node. So new distance is distance to upcoming node which is the decimal value
  rd     = unDistance p `mod'` 1

  n      = uncurry Pn
  final p = if canPass $ get $ step p then final (step p) else p
  get (x, y) = level !! y !! x
  canPass Empty               = True
  canPass (Collectible _ _ _) = True
  canPass _                   = False
  level = getGridItems grid
