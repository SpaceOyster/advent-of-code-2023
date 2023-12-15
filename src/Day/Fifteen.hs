{-# LANGUAGE OverloadedStrings #-}

module Day.Fifteen (solution1, solution2) where

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T

hash :: String -> Int
hash = foldl (\acc c -> ((acc + C.ord c) * 17) `mod` 256) 0

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let answer = sum $ hash . filter (/= '\n') . T.unpack <$> T.splitOn "," contents
  print answer

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
