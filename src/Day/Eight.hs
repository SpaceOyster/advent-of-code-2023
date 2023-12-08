{-# LANGUAGE OverloadedStrings #-}

module Day.Eight (solution1, solution2) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Node = Node
  { nName :: T.Text,
    nLeft :: T.Text,
    nRight :: T.Text
  }
  deriving (Show, Eq)

data Instruction = L | R
  deriving (Show, Eq, Ord, Enum)

data List = List
  { lInstructions :: [Instruction],
    lMap :: Map.Map T.Text Node
  }
  deriving (Show)

parseInstructions :: T.Text -> [Instruction]
parseInstructions = fmap f . T.unpack
  where
    f 'L' = L
    f 'R' = R

parseNode :: (Applicative f) => T.Text -> f Node
parseNode text = do
  let [nName, pair] = T.splitOn " = " text
      [nLeft, nRight] = T.splitOn ", " $ T.tail $ T.init pair
  pure $ Node {nName, nLeft, nRight}

parseMap :: T.Text -> Maybe (Map.Map T.Text Node)
parseMap text = do
  let ls = T.lines text
  nodes <- traverse parseNode ls
  Just $ Map.fromList $ fmap (\n -> (nName n, n)) nodes

parseList :: T.Text -> Maybe List
parseList text = do
  let [insts, m] = T.splitOn "\n\n" text
      lInstructions = parseInstructions insts
  lMap <- parseMap m
  Just $ List {lInstructions, lMap}

followInstruction :: Map.Map T.Text Node -> Node -> Instruction -> Maybe Node
followInstruction m n i =
  case i of
    L -> Map.lookup (nLeft n) m
    R -> Map.lookup (nRight n) m

findZZZ :: Map.Map T.Text Node -> [Instruction] -> Maybe Int
findZZZ m is = do
  n <- Map.lookup "AAA" m
  go n (cycle is) 0
  where
    go :: Node -> [Instruction] -> Int -> Maybe Int
    go n is' s = case nName n of
      "ZZZ" -> pure s
      _ -> followInstruction m n (head is') >>= \n' -> go n' (drop 1 is') (s + 1)

searchZZZ :: List -> Maybe Int
searchZZZ l = findZZZ (lMap l) (lInstructions l)

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let listM = parseList contents
  print $ listM >>= searchZZZ

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
