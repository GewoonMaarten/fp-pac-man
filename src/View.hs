module View
  ( drawView
  )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Controllers.Level
import           Model
import           Models.PacMan
import           Models.Ghost

drawView :: GameState -> Picture
drawView gameState = draw (unScene gameState) gameState

textScale :: Picture -> Picture
textScale = let s = 0.25 in scale s s

draw :: Scene -> GameState -> Picture
draw Play     gameState = pictures 
  $  (showGrid $ unLevel gameState)
  ++ [showGhost g | g <- unGhosts gameState]
  ++ [showPacMan $ unPacMan gameState]
draw Home     _         = textScale $ color white $ text "Home"
draw Pause    _         = textScale $ color white $ text "Pause"
draw gameOver _         = textScale $ color white $ text "Game Over"
