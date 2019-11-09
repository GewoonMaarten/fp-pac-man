module Utils.ScoreBoard
  ( setScore
  , getScores
  )
where

import           System.IO
import           Config
import           Paths_PacMan
import           Data.List.Split

type Score = (Int, String)

setScore :: Score -> IO ()
setScore (scr, str) = do
  filePath <- getDataFileName scoreBoardFilePath
  appendFile filePath (str ++ ", " ++ show scr ++ "\n")

getScores :: IO [Score]
getScores = do
  filePath <- getDataFileName scoreBoardFilePath
  contents <- readFile filePath
  return map (toTuple . map read . splitOn ", ") lines contents
  where toTuple [x, y] = (x, y)
