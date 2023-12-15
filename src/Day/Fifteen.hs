{-# LANGUAGE OverloadedStrings #-}

module Day.Fifteen (solution1, solution2) where

import qualified Data.Char as C
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

hash :: String -> Int
hash = foldl (\acc c -> (acc + C.ord c) * 17 `mod` 256) 0

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let answer = sum $ hash . filter (/= '\n') . T.unpack <$> T.splitOn "," contents
  print answer

type Boxes = Map.Map Int [Lens]

data Lens = Lens {lLabel :: String, lFocal :: Int}
  deriving (Show, Eq)

data Instruction = Update Lens | Remove String
  deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction s
  | '=' `elem` s = let (l, n) = break (== '=') s in Update (Lens l (read $ drop 1 n))
  | otherwise = let (l, _) = break (== '-') s in Remove l

parseInstructions :: T.Text -> [Instruction]
parseInstructions text = parseInstruction . filter (/= '\n') . T.unpack <$> T.splitOn "," text

readBoxLabel :: Instruction -> Int
readBoxLabel (Update l) = hash (lLabel l)
readBoxLabel (Remove l) = hash l

updateList :: Instruction -> [Lens] -> [Lens]
updateList (Update l) ls = let (a, b) = break ((== lLabel l) . lLabel) ls in a <> (l : drop 1 b)
updateList (Remove l) ls = filter ((/= l) . lLabel) ls

runInstruction :: Boxes -> Instruction -> Boxes
runInstruction bs i =
  let label = readBoxLabel i
      ls = fromMaybe [] $ Map.lookup label bs
   in Map.insert (readBoxLabel i) (updateList i ls) bs

runInstructions :: Boxes -> [Instruction] -> Boxes
runInstructions = foldl runInstruction

sumFocusingPower :: Boxes -> Int
sumFocusingPower = Map.foldrWithKey (\k ls acc -> (k + 1) * listFocalPower ls + acc) 0
  where
    listFocalPower ls = sum $ zipWith (\i l -> i * lFocal l) [1 ..] ls

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let instructions = parseInstructions contents
      afterInstrs = runInstructions Map.empty instructions
      answer = sumFocusingPower afterInstrs
  print answer
