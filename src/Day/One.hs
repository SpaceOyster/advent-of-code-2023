{-# LANGUAGE OverloadedStrings #-}

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

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let ls = filter (/= "") $ T.splitOn "\n" contents :: [T.Text]
      calibrations = parseWords <$> ls
      answer = sum calibrations
  print answer
