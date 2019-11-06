-- hs-boot to prevent recursion
module Controllers.SceneController
  ( getScene
  )
where
import           Model
getScene :: SceneType -> Scene