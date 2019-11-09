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
import           Utils.Play
import           Data.List.Split
import           Utils.ScoreBoard

inputHandler :: Scene -> Event -> GameState -> IO GameState
--------------------------------------------------------------------------------
-- Scene: Home
--------------------------------------------------------------------------------
inputHandler Home (EventKey (SpecialKey KeyEnter) Up _ _) gameState = do
  filePath  <- getDataFileName "assets/map/map.csv"
  csvString <- readFile filePath
  return (initialPlay (level csvString) gameState)
 where
  level :: String -> [[Int]]
  level = map (map t . splitOn ",") . lines
  t :: String -> Int
  t = read
--------------------------------------------------------------------------------
-- Scene: Play
--------------------------------------------------------------------------------
inputHandler Play (EventKey (SpecialKey KeyLeft) _ _ _) gameState =
  return $ moveFn (\(x, y) -> (x - 1, y)) gameState
-- Right Key
inputHandler Play (EventKey (SpecialKey KeyRight) _ _ _) gameState =
  return $ moveFn (\(x, y) -> (x + 1, y)) gameState
-- Up Key
inputHandler Play (EventKey (SpecialKey KeyUp) _ _ _) gameState =
  return $ moveFn (\(x, y) -> (x, y - 1)) gameState
-- Down Key
inputHandler Play (EventKey (SpecialKey KeyDown) _ _ _) gameState =
  return $ moveFn (\(x, y) -> (x, y + 1)) gameState
inputHandler Play (EventKey (SpecialKey KeySpace) Up _ _) gameState =
  return gameState { unScene = Pause }
--------------------------------------------------------------------------------
-- Scene: Pause
--------------------------------------------------------------------------------
inputHandler Pause (EventKey (SpecialKey KeyEnter) Up _ _) gameState =
  return gameState { unScene = Home }
inputHandler Pause (EventKey (SpecialKey KeySpace) Up _ _) gameState =
  return gameState { unScene = Play }
--------------------------------------------------------------------------------
-- Scene: GameOver
--------------------------------------------------------------------------------
inputHandler (GameOver str) (EventKey (SpecialKey KeyEnter) Up _ _) gameState =
  do
    setScore ("test", 0)
    setScore (str, unScore $ unPacMan gameState)
    return gameState { unScene = Home }
inputHandler (GameOver str) (EventKey (SpecialKey KeyBackspace) Up _ _) gameState
  | not (null str)
  = return gameState { unScene = GameOver (init str) }
  | otherwise
  = return gameState
inputHandler (GameOver str) (EventKey (Char c) Up _ _) gameState
  | length str <= 20 = return gameState { unScene = GameOver (str ++ [c]) }
  | otherwise        = return gameState
--------------------------------------------------------------------------------
inputHandler _ _ gameState = return gameState
--------------------------------------------------------------------------------

moveFn :: StepFn -> GameState -> GameState
moveFn dirFn gameState =
  gameState { unPacMan = move (unLevel gameState) dirFn (unPacMan gameState) }

move :: Grid -> StepFn -> PacMan -> PacMan
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
