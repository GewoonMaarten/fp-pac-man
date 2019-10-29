module View
  ( drawView
  )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Controllers.Level
import           Model
import           PacMan

drawView :: GameState -> Picture
drawView gameState = draw (unScene gameState) gameState

textScale :: Picture -> Picture
textScale = let s = 0.25 in scale s s

draw :: Scene -> GameState -> Picture
draw Play     gameState = pictures ((showGrid $ unLevel gameState) ++ [showPacMan (unPacMan gameState)])
draw Home     _         = textScale $ color white $ text "Home"
draw Pause    _         = textScale $ color white $ text "Pause"
draw gameOver _         = textScale $ color white $ text "Game Over"

showPacMan (PacMan pns d) = translate x (-y) $ color yellow $ circleSolid 10
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pns d))
