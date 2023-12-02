{-# LANGUAGE OverloadedStrings #-}

{-
--- Day 1: Trebuchet?! ---

Something is wrong with global snow production, and you've been selected to take
a look. The Elves have even given you a map; on it, they've used stars to mark
the top fifty locations that are likely to be having problems.

You've been doing this long enough to know that to restore snow operations, you
need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You try to ask why they can't just use a weather machine ("not powerful enough")
and where they're even sending you ("the sky") and why your map looks mostly
blank ("you sure ask a lot of questions") and hang on did you just say the sky
("of course, where do you think snow comes from") when you realize that the
Elves are already loading you into a trebuchet ("please hold still, we need to
strap you in").

As they're making the final adjustments, they discover that their calibration
document (your puzzle input) has been amended by a very young Elf who was
apparently just excited to show off her art skills. Consequently, the Elves
are having trouble reading the values on the document.

The newly-improved calibration document consists of lines of text; each line
originally contained a specific calibration value that the Elves now need to
recover. On each line, the calibration value can be found by combining the
first digit and the last digit (in that order) to form a single two-digit number.

For example:

1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet

In this example, the calibration values of these four lines are 12, 38, 15,
and 77. Adding these together produces 142.

Consider your entire calibration document. What is the sum of all of the
calibration values?
-}

module Day.One (solution1, solution2) where

import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as T

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let ls = filter (/= "") $ T.splitOn "\n" contents :: [T.Text]
      digits = fmap (T.filter isDigit) ls :: [T.Text]
      calibrations = fmap (\t -> read [T.head t, T.last t] :: Integer) digits :: [Integer]
      answer = sum calibrations
  print answer

substs :: [(T.Text, T.Text)]
substs =
  [ ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")
  ]

-- non optimal: we don't need to make all the substitutions, wee only need to
-- scan for the first and the last
parseWords :: T.Text -> Integer
parseWords line =
  (\t -> read [T.head t, T.last t] :: Integer) $ T.filter isDigit $ ping line
  where
    ping :: T.Text -> T.Text
    ping "" = ""
    ping l =
      case pong l of
        [] -> T.pack [T.head l] <> ping (T.tail l)
        x : _ -> x <> ping (T.tail l)
    pong :: T.Text -> [T.Text]
    pong "" = []
    pong l = do
      (n, r) <- substs
      case T.stripPrefix n l of -- stripPrefix automatically checks if n is prefiex of l
        Just _ -> pure r -- when we get Just we know that n really was a prefix of l
        Nothing -> [] -- when we get Nothing, well... n wasn't a prefix of l

{-
--- Part Two ---

Your calculation isn't quite right. It looks like some of the digits are
actually spelled out with letters: one, two, three, four, five, six, seven,
eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real first and last
digit on each line. For example:

two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen

In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76.
Adding these together produces 281.

What is the sum of all of the calibration values?
-}

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let ls = filter (/= "") $ T.splitOn "\n" contents :: [T.Text]
      calibrations = parseWords <$> ls
      answer = sum calibrations
  print answer
