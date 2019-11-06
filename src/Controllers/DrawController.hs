module Controllers.DrawController where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Model
import           Models.PacMan
import           Models.Ghost
import           Models.Level
import           Utils.Graphics

drawHome, drawPlay :: Textures -> GameState -> Picture
drawPlay ts@(Textures _ _ gt gat pt pdt _) gameState =
  pictures
    $  (showGrid $ unLevel gameState)
    ++ [ drawGhost g ts | g <- unGhosts gameState ]
    ++ [showPacMan pt $ unPacMan gameState]
    ++ [showScore $ unPacMan gameState]
    ++ [showLives ts $ unPacMan gameState]

drawHome ts _ = pictures [banner, menuText, startText, quitText]
 where
  banner = translate 0 150 $ scale 0.4 0.4 $ textureBanner ts
  menuText =
    translate (-120) 50 $ color white $ scale 0.35 0.35 $ text "Start Menu"
  startText = translate (-50) 0 $ color white $ scale 0.2 0.2 $ text "> Start"
  quitText =
    translate (-50) (-35) $ color white $ scale 0.2 0.2 $ text "  Quit"

drawPause _ gs = blank
drawGameOver _ gs = blank
