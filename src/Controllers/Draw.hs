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
       , txtsToPic
         (floatLeft 20)
         (-240)
         [ ("Use the arrow keys to move around", Smallest)
         , ("Press \"Space\" to pause"         , Smallest)
         ]
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
    [ ("Menu"                    , Medium)
    , ("Press \"Enter\" to start", Small)
    , ("Press \"Esc\" to quit"   , Small)
    , (""                        , Small)
    , (""                        , Small)
    , (""                        , Small)
    , (""                        , Small)
    , ("By Tom de Goede"         , Smallest)
    , ("and Maarten de Klerk"    , Smallest)
    , (""                        , Small)
    , ("Version 1, 2019"         , Smallest)
    ]
--------------------------------------------------------------------------------
-- Scene: Pause
--------------------------------------------------------------------------------
drawScene Pause _ _ = txtsToPic
  (floatLeft 40)
  0
  [ ("Pause"                        , Large)
  , ("Press \"Space\" to unpause"   , Smallest)
  , ("Press \"Enter\" to go to menu", Smallest)
  ]
--------------------------------------------------------------------------------
-- Scene: GameOver
--------------------------------------------------------------------------------
drawScene GameOver _ gameState =
  let pacMan      = unPacMan gameState
      gameOverStr = if unLives pacMan == 0 then "Game Over" else "You Win!"
  in  txtsToPic
        (floatLeft 40)
        0
        [ (gameOverStr                    , Medium)
        , ("Your score: " ++ show (unScore pacMan), Small)
        , (""                             , Small)
        , ("Press \"Enter\" to go to menu", Smallest)
        ]
