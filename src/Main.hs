module Main (main) where

import qualified Day.Eight
import qualified Day.Eleven
import qualified Day.Fifteen
import qualified Day.Five
import qualified Day.Four
import qualified Day.Fourteen
import qualified Day.Nine
import qualified Day.One
import qualified Day.Seven
import qualified Day.Six
import qualified Day.Ten
import qualified Day.Thirteen
import qualified Day.Three
import qualified Day.Twelve
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
    "5.1" -> Day.Five.solution1 inputFile
    "5.2" -> Day.Five.solution2 inputFile
    "6.1" -> Day.Six.solution1 inputFile
    "6.2" -> Day.Six.solution2 inputFile
    "7.1" -> Day.Seven.solution1 inputFile
    "7.2" -> Day.Seven.solution2 inputFile
    "8.1" -> Day.Eight.solution1 inputFile
    "8.2" -> Day.Eight.solution2 inputFile
    "9.1" -> Day.Nine.solution1 inputFile
    "9.2" -> Day.Nine.solution2 inputFile
    "10.1" -> Day.Ten.solution1 inputFile
    "10.2" -> Day.Ten.solution2 inputFile
    "11.1" -> Day.Eleven.solution1 inputFile
    "11.2" -> Day.Eleven.solution2 inputFile
    "12.1" -> Day.Twelve.solution1 inputFile
    "12.2" -> Day.Twelve.solution2 inputFile
    "13.1" -> Day.Thirteen.solution1 inputFile
    "13.2" -> Day.Thirteen.solution2 inputFile
    "14.1" -> Day.Fourteen.solution1 inputFile
    "14.2" -> Day.Fourteen.solution2 inputFile
    "15.1" -> Day.Fifteen.solution1 inputFile
    "15.2" -> Day.Fifteen.solution2 inputFile
    _ -> print $ "no solution with index " <> show day
