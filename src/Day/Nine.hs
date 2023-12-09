{-# LANGUAGE OverloadedStrings #-}

module Day.Nine (solution1, solution2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Sequence = [Int]

parseSequences :: T.Text -> [Sequence]
parseSequences = fmap (fmap (read . T.unpack) . T.words) . T.lines

findNextRow :: Sequence -> Sequence
findNextRow s = zipWith (-) (drop 1 s) s

findAllRows :: Sequence -> [Sequence]
findAllRows = takeWhile (not . all (== 0)) . iterate findNextRow

findNextNumber :: Sequence -> Int
findNextNumber s = foldl f 0 $ findAllRows s
  where
    f p l = last l + p

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let sequences = parseSequences contents
      nextNumbers = findNextNumber <$> sequences
  print $ sum nextNumbers
