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
import           Utils.Path
import           Utils.ScoreBoard

drawScene :: Scene -> Textures -> [Score] -> GameState -> Picture
--------------------------------------------------------------------------------
-- Scene: Play
--------------------------------------------------------------------------------
drawScene Play ts@(Textures _ _ gt gat pt pdt _) _ gameState =
  pictures
    $  showGrid ts (unLevel gameState)
    ++ [ drawGhost g ts | g <- unGhosts gameState ]
    ++ drawDebug
    ++ [ showPacMan pt pm
       , showScore pm
       , showLives ts pm
       , txtsToPic
         (floatLeft 20)
         (-240)
         [ ("Use the arrow keys to move around", Smallest)
         , ("Press \"Esc\" to pause"           , Smallest)
         ]
       ]
  where
    pm = unPacMan gameState 
    drawDebug = if unDebug gameState then 
      [drawPath green $ unPath pm]
      ++  [ (drawPath red . unPathG) g | g <- unGhosts gameState ]
      else []
--------------------------------------------------------------------------------
-- Scene: Home
--------------------------------------------------------------------------------
drawScene Home ts _ _ = pictures [banner, txts]
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
drawScene Pause _ _ _ = txtsToPic
  (floatLeft 40)
  0
  [ ("Pause"                        , Large)
  , (""                             , Small)
  , ("Press \"Esc\" to unpause"   , Smallest)
  , ("Press \"Enter\" to go to menu", Smallest)
  ]
--------------------------------------------------------------------------------
-- Scene: GameOver
--------------------------------------------------------------------------------
drawScene (GameOver name) _ scores gameState =
  let pacMan      = unPacMan gameState
      gameOverStr = if unLives pacMan == 0 then "Game Over" else "You Win!"
      topThree = formatScores scores
  in  txtsToPic
        (floatLeft 40)
        150
        (  [ (gameOverStr, Medium)
           , (""         , Small)
           , (""         , Small)
           , ("Top 3"    , Small)
           ]
        ++ topThree
        ++ [ (""                             , Smallest)
           , (""                             , Smallest)
           , ("Your score: " ++ show (unScore pacMan), Small)
           , ("Enter your name: "            , Smallest)
           , (name                           , Smallest)
           , (""                             , Smallest)
           , (""                             , Smallest)
           , (""                             , Smallest)
           , ("Press \"Enter\" to go to menu", Smallest)
           , ("Press \"Esc\" to quit"        , Smallest)
           ]
        )
