module View
  ( drawView
  )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Model
import           Models.PacMan
import           Models.Ghost
import           Models.Level
import           Graphics

drawView :: GameState -> Textures -> Picture
drawView gameState = draw (unScene gameState) gameState

textScale :: Picture -> Picture
textScale = let s = 0.25 in scale s s

draw :: Scene -> GameState -> Textures -> Picture
draw Play     gameState t = drawPlay gameState t
draw Home     _         t = drawHome t
draw Pause    _         _ = textScale $ color white $ text "Pause"
draw gameOver _         _ = textScale $ color white $ text "Game Over"


drawPlay :: GameState -> Textures -> Picture
drawPlay gameState ts@(Textures _ _ gt gat pt pdt _) =
  pictures
    $  (showGrid $ unLevel gameState)
    ++ [ drawGhost g ts | g <- unGhosts gameState ]
    ++ [showPacMan pt $ unPacMan gameState]
    ++ [showScore $ unPacMan gameState]
    ++ [showLives ts $ unPacMan gameState]

drawHome :: Textures -> Picture
drawHome ts = pictures [banner, menuText, startText, quitText]
 where
  banner = translate 0 150 $ scale 0.4 0.4 $ textureBanner ts
  menuText =
    translate (-120) 50 $ color white $ scale 0.35 0.35 $ text "Start Menu"
  startText = translate (-50) 0 $ color white $ scale 0.2 0.2 $ text "> Start"
  quitText =
    translate (-50) (-35) $ color white $ scale 0.2 0.2 $ text "  Quit"
