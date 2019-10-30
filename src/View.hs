module View
  ( drawView
  )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Controllers.Level
import           Model
import           Models.PacMan
import           Models.Ghost
import           Graphics

drawView :: GameState -> Textures -> Picture
drawView gameState t = draw (unScene gameState) gameState t

textScale :: Picture -> Picture
textScale = let s = 0.25 in scale s s

draw :: Scene -> GameState -> Textures -> Picture
draw Play     gameState t = drawPlay gameState t
draw Home     _         _ = textScale $ color white $ text "Home"
draw Pause    _         _ = textScale $ color white $ text "Pause"
draw gameOver _         _ = textScale $ color white $ text "Game Over"


drawPlay :: GameState -> Textures -> Picture
drawPlay gameState (Textures _ _ gt gat pt pdt) =
  pictures
    $  (showGrid $ unLevel gameState)
    ++ [ drawGhost g (ghostPos g) gt | g <- unGhosts gameState ]
    ++ [showPacMan pt $ unPacMan gameState]
    ++ [showScore $ unPacMan gameState]


drawGhost :: Ghost -> (Float, Float) -> [(GhostType, TextureSet)] -> Picture
drawGhost _ _ [] = blank
drawGhost g@(Ghost _ gt False) (x, y) (t : ts)
  | gt == fst t
  = let (GhostTextureSet p1 _) = snd t
        s                      = 0.6
    in  translate x (-y) $ scale s s $ p1
  | otherwise
  = drawGhost g (x, y) ts
