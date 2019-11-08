module Controllers.Input
  ( inputHandler
  )
where

import           Config
import           Data.Fixed                     ( mod' )
import           Graphics.Gloss.Interface.IO.Game
import           Model
import           Models.Ghost
import           Models.Level
import           Models.PacMan
import           Utils.Path

inputHandler :: Scene -> Event -> GameState -> GameState
--------------------------------------------------------------------------------
-- Scene: Home
--------------------------------------------------------------------------------
inputHandler Home (EventKey (SpecialKey KeyEnter) Up _ _) gameState = gameState
  { unScene  = Play
  , unLevel  = buildGrid gridX gridY gridSize
  , unGhosts = [ initialGhost Blinky
               , initialGhost Inky
               , initialGhost Pinky
               , initialGhost Clyde
               ]
  }
--------------------------------------------------------------------------------
-- Scene: Play
--------------------------------------------------------------------------------
inputHandler Play (EventKey (SpecialKey KeyLeft) _ _ _) gameState =
  moveFn (\(x, y) -> (x - 1, y)) gameState
-- Right Key
inputHandler Play (EventKey (SpecialKey KeyRight) _ _ _) gameState =
  moveFn (\(x, y) -> (x + 1, y)) gameState
-- Up Key
inputHandler Play (EventKey (SpecialKey KeyUp) _ _ _) gameState =
  moveFn (\(x, y) -> (x, y - 1)) gameState
-- Down Key
inputHandler Play (EventKey (SpecialKey KeyDown) _ _ _) gameState =
  moveFn (\(x, y) -> (x, y + 1)) gameState
inputHandler Play (EventKey (SpecialKey KeyEsc) Up _ _) gameState =
  gameState { unScene = Pause }
--------------------------------------------------------------------------------
-- Scene: Pause
--------------------------------------------------------------------------------
inputHandler Pause (EventKey (SpecialKey KeyEnter) Up _ _) gameState =
  gameState { unScene = Home }
inputHandler Pause (EventKey (SpecialKey KeyEsc) Up _ _) gameState =
  gameState { unScene = Play }
--------------------------------------------------------------------------------
-- Scene: GameOver
--------------------------------------------------------------------------------
inputHandler GameOver (EventKey (SpecialKey KeyEnter) Up _ _) gameState =
  gameState { unScene = Home }
--------------------------------------------------------------------------------
inputHandler _ _ gameState = gameState
--------------------------------------------------------------------------------


moveFn dirFn gameState =
  gameState { unPacMan = move (unLevel gameState) dirFn (unPacMan gameState) }

move :: Grid -> ((Int, Int) -> (Int, Int)) -> PacMan -> PacMan
move grid step pm = if upN /= head followed then mutate else pm
 where
  p           = unPath pm
  mutate      = pm { unPath = P newPath newDistance }
  -- combine current origin (head) with new followed path
  newPath     = head (unNodes p) : upN : followed
  upN         = upcomingNode p
  followed    = follow (canPass . getGridItem grid) (const False) step upN
  -- new target is upcoming node. So new distance is distance to upcoming node
  -- which is the decimal value
  newDistance = unDistance p `mod'` 1
