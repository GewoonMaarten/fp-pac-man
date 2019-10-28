module View
  ( drawView
  )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt
import           Controllers.Level
import           Model

drawView :: GameState -> Picture
drawView gameState = draw (unScene gameState) gameState

textScale :: Picture -> Picture
textScale = let s = 0.25 in scale s s

draw :: Scene -> GameState -> Picture
draw Play     gameState = pictures ((showGrid (unLevel gameState) (-180) (-175)) ++ [showPacMan (unPacMan gameState)])
draw Home     _         = textScale $ color white $ text "Home"
draw Pause    _         = textScale $ color white $ text "Pause"
draw gameOver _         = textScale $ color white $ text "Game Over"

showPacMan pm = uncurry translate (actual pm) $ color yellow $ circleSolid 10

actual (PacMan [p] _) = toVector p
actual (PacMan (a:b:_) d) = vb Pt.+ (d Pt.* n)
  where
    vb = toVector b
    delta = (toVector a) Pt.- vb
    n = normalizeV delta

toVector :: PathNode -> Vector
toVector (Pn x y) = (fromIntegral x, fromIntegral y)
