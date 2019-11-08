module Controllers.Draw
  ( drawScene
  )
where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Graphics.Gloss.Data.Vector
import           Model
import           Models.Ghost
import           Models.Level
import           Models.PacMan
import           Utils.Graphics
import           Utils.Text

drawScene :: Scene -> Textures -> GameState -> Picture
--------------------------------------------------------------------------------
-- Scene: Play
--------------------------------------------------------------------------------
drawScene Play ts@(Textures _ _ gt gat pt pdt _) gameState =
  pictures
    $  showGrid (unLevel gameState)
    ++ [ drawGhost g ts | g <- unGhosts gameState ]
    ++ [ showPacMan pt $ unPacMan gameState
       , showScore $ unPacMan gameState
       , showLives ts $ unPacMan gameState
       ]
--------------------------------------------------------------------------------
-- Scene: Home
--------------------------------------------------------------------------------
drawScene Home ts _ = pictures [banner, txts]
 where
  banner = translate 0 150 $ scale 0.4 0.4 $ textureBanner ts
  txts   = txtsToPic
    (floatLeft 40)
    0
    [("Start Menu", Medium), ("> Start", Small), ("Quit", Small)]
--------------------------------------------------------------------------------
-- Scene: Pause
--------------------------------------------------------------------------------
drawScene Pause    _ _ = translate 0 0 $ color white $ text "Pause"
--------------------------------------------------------------------------------
-- Scene: GameOver
--------------------------------------------------------------------------------
drawScene GameOver _ _ = translate 0 0 $ color white $ text "GameOver"
