module Controllers.SceneController
  ( getScene
  )
where

import           Model
import           Controllers.DrawController
import           Controllers.KeyController
import           Controllers.UpdateController

homeScene, playScene, pauseScene, gameOverScene :: Scene
homeScene = Scene { sceneType   = Home
                  , sceneDraw   = drawHome
                  , sceneInput  = inputHome
                  , sceneUpdate = updateBase
                  }
playScene = Scene { sceneType   = Play
                  , sceneDraw   = drawPlay
                  , sceneInput  = inputPlay
                  , sceneUpdate = updatePlay
                  }
pauseScene = Scene { sceneType   = Pause
                   , sceneDraw   = drawPause
                   , sceneInput  = inputPause
                   , sceneUpdate = updateBase
                   }
gameOverScene = Scene { sceneType   = GameOver
                      , sceneDraw   = drawGameOver
                      , sceneInput  = inputGameOver
                      , sceneUpdate = updateBase
                      }

getScene :: SceneType -> Scene
getScene Home     = homeScene
getScene Play     = playScene
getScene Pause    = pauseScene
getScene GameOver = gameOverScene
