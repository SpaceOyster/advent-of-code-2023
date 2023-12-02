{-# LANGUAGE OverloadedStrings #-}

module Day.One (solution1, solution2) where

import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as T

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let ls = filter (/= "") $ T.splitOn "\n" contents
      digits = fmap (T.filter isDigit) ls
      calibrations = fmap (\t -> read [T.head t, T.last t] :: Integer) digits
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

parseWords :: T.Text -> Integer
parseWords line =
  (\t -> read [T.head t, T.last t] :: Integer) $ T.filter isDigit $ ping line
  where
    ping "" = ""
    ping l =
      case pong l of
        [] -> T.pack [T.head l] <> ping (T.tail l)
        x : _ -> x <> ping (T.tail l)
    pong :: T.Text -> [T.Text]
    pong "" = []
    pong l = do
      (n, r) <- substs
      case T.stripPrefix n l of
        Just _ -> pure r
        Nothing -> []

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let ls = filter (/= "") $ T.splitOn "\n" contents
      calibrations = parseWords <$> ls
      answer = sum calibrations
  print answer
