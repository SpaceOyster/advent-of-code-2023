module Main (main) where

import qualified Day.One
import qualified System.Environment as E

main :: IO ()
main = do
  day : inputFile : _ <- E.getArgs
  case day of
    "1.1" -> Day.One.solution1 inputFile
    "1.2" -> Day.One.solution2 inputFile
    _ -> print $ "no solution with index " <> show day
