module Main where

import           Config
import           Model
import           Models.PacMan
import           Models.Ghost
import           Controllers.Animator
import           Controllers.Input              ( inputHandler )
import           Controllers.Draw               ( drawScene )
import           Controllers.Update             ( updateScene )
import           Utils.Collectible
import           Utils.Path
import           Utils.Graphics
import           Utils.ScoreBoard

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Interface.Environment
import           System.Random

main :: IO ()
main = do
    textures                    <- loadTextures
    scores                      <- getScores
    (screenWidth, screenHeight) <- getScreenSize
    playIO
        (InWindow
            "Pac-Man"
            (windowWidth, windowHeight)
            ( (screenWidth `div` 2) - (windowWidth `div` 2)
            , (screenHeight `div` 2) - (windowHeight `div` 2)
            )
        ) -- Display mode
        black -- Background Color
        30 -- Number of steps per second
        intialGameState -- Initial world
        (draw textures scores) -- (world -> IO Picture)
        input -- (Event -> world -> IO world)
        update -- (Float -> world -> IO world)

intialGameState :: GameState
intialGameState = GameState Home (Grid [] 0 0 0) initialPacMan [] (mkStdGen 1) True

draw :: Textures -> [Score] -> GameState -> IO Picture
draw textures scores gs = return (drawScene (unScene gs) textures scores gs)

input :: Event -> GameState -> IO GameState
input event gs = inputHandler (unScene gs) event gs

update :: Float -> GameState -> IO GameState
update dt gs = return (updateScene (unScene gs) dt gs)
