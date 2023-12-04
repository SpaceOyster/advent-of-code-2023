module Main (main) where

import qualified Day.Four
import qualified Day.One
import qualified Day.Three
import qualified Day.Two
import qualified System.Environment as E

main :: IO ()
main = do
  day : inputFile : _ <- E.getArgs
  case day of
    "1.1" -> Day.One.solution1 inputFile
    "1.2" -> Day.One.solution2 inputFile
    "2.1" -> Day.Two.solution1 inputFile
    "2.2" -> Day.Two.solution2 inputFile
    "3.1" -> Day.Three.solution1 inputFile
    "3.2" -> Day.Three.solution2 inputFile
    "4.1" -> Day.Four.solution1 inputFile
    "4.2" -> Day.Four.solution2 inputFile
    _ -> print $ "no solution with index " <> show day
