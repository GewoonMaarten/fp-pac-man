module Controllers.KeyController
  ( inputHandler
  )
where

import           Model
import           Models.PacMan
import           Utils.Path
import           Models.Level
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
move grid step pm@(PacMan p _ _ _) = if upN /= head followed then mutate else pm
 where
  mutate = pm { unPath = P newPath newDistance }
  -- combine current origin (head) with new followed path
  newPath = (head $ unNodes p) : (map (uncurry Pn) (upN : followed))
  upN = upcomingNode p
  followed = follow upN
  -- new target is upcoming node. So new distance is distance to upcoming node which is the decimal value
  newDistance = unDistance p `mod'` 1

  follow p = f (wrap $ step p) (canPass $ get $ step p) 
    where
      -- wrap at bounds 
      f (Just wrapped) _    = p : wrapped : follow wrapped
      -- move forward
      f _              True = follow (step p)
      -- finish
      f _              _    = [p]

  -- wrap boundary nodes to the other side
  wrap (-1, y) = Just (18, y)
  wrap (19, y) = Just (0, y)
  wrap (x, -1) = Just (x, 20)
  wrap (x, 21) = Just (x, 0)
  wrap _ = Nothing

  get (x, y) = level !! y !! x
  canPass Empty               = True
  canPass Collectible{}       = True
  canPass _                   = False
  level = getGridItems grid
