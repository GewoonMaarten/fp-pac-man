module Controllers.KeyController where

import           Model
import           Models.PacMan
import           Utils.Path
import           Models.Level
import           Models.Ghost
import           Graphics.Gloss.Interface.IO.Game
import           Data.Fixed                     ( mod' )
import {-# SOURCE #-} Controllers.SceneController
                                                ( getScene )

inputHome :: Event -> GameState -> GameState
inputHome (EventKey (SpecialKey KeyEnter) _ _ _) gameState = gameState
  { unScene  = getScene Play
  , unLevel  = buildGrid (-180) 255 20
  , unGhosts = [ initialGhost Blinky
               , initialGhost Inky
               , initialGhost Pinky
               , initialGhost Clyde
               ]
  }
inputHome _ gameState = gameState

inputPlay :: Event -> GameState -> GameState
inputPlay (EventKey (SpecialKey KeyLeft) _ _ _) gameState =
  moveFn (\(x, y) -> (x - 1, y)) gameState
-- Right Key
inputPlay (EventKey (SpecialKey KeyRight) _ _ _) gameState =
  moveFn (\(x, y) -> (x + 1, y)) gameState
-- Up Key
inputPlay (EventKey (SpecialKey KeyUp) _ _ _) gameState =
  moveFn (\(x, y) -> (x, y - 1)) gameState
-- Down Key
inputPlay (EventKey (SpecialKey KeyDown) _ _ _) gameState =
  moveFn (\(x, y) -> (x, y + 1)) gameState
inputPlay _ gameState = gameState

moveFn dirFn gameState =
  gameState { unPacMan = move (unLevel gameState) dirFn (unPacMan gameState) }

move :: Grid -> ((Int, Int) -> (Int, Int)) -> PacMan -> PacMan
move grid step pm = if upN /= head followed then mutate else pm
 where
  p           = unPath pm
  mutate      = pm { unPath = P newPath newDistance }
  -- combine current origin (head) with new followed path
  newPath     = (head $ unNodes p) : upN : followed
  upN         = upcomingNode p
  followed    = follow (canPass . getGridItem grid) (const False) step upN
  -- new target is upcoming node. So new distance is distance to upcoming node which is the decimal value
  newDistance = unDistance p `mod'` 1


inputPause :: Event -> GameState -> GameState
inputPause (EventKey (SpecialKey KeyEnter) _ _ _) gameState =
  gameState { unScene = getScene Home }
inputPause (EventKey (SpecialKey KeyEsc) _ _ _) gameState =
  gameState { unScene = getScene Play }
inputPause _ gameState = gameState

inputGameOver :: Event -> GameState -> GameState
inputGameOver (EventKey (SpecialKey KeyEnter) _ _ _) gameState =
  gameState { unScene = getScene Home }
inputGameOver _ gameState = gameState
