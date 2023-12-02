{-# LANGUAGE OverloadedStrings #-}

module Day.Two (solution1, solution2) where

import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype CubesNumber = CubesNumber {getCubesNumber :: Integer}
  deriving (Num, Eq, Ord, Enum, Show, Real, Integral)

data SetOfCubes = SetOfCubes
  { socRed :: CubesNumber,
    socGreen :: CubesNumber,
    socBlue :: CubesNumber
  }
  deriving (Eq, Show)

instance Ord SetOfCubes where
  a <= b = (socRed a <= socRed b) && (socGreen a <= socGreen b) && (socBlue a <= socBlue b)

instance Semigroup SetOfCubes where
  a <> b =
    SetOfCubes
      { socRed = socRed a + socRed b,
        socGreen = socGreen a + socGreen b,
        socBlue = socBlue a + socBlue b
      }

type Turn = SetOfCubes

data Game = Game
  { gNumber :: Integer,
    gSequence :: [Turn]
  }
  deriving (Show)

isGamePossible :: Game -> SetOfCubes -> Bool
isGamePossible game setOfCubes = all (< setOfCubes) $ gSequence game

findPossible :: [Game] -> SetOfCubes -> [Game]
findPossible game setOfCubes = [g | g <- game, g `isGamePossible` setOfCubes]

-- it's tempting to use parsec here
parseGames :: T.Text -> [Game]
parseGames text = do
  line <- T.lines text
  maybeToList $ parseGame line

parseGame :: T.Text -> Maybe Game
parseGame line = do
  let (gameNumS, gameStr') = T.breakOn ": " line
  gNumber <- parseGameNumber gameNumS
  gameStr <- T.stripPrefix ": " gameStr'
  gSequence <- traverse parseATurn $ T.splitOn "; " gameStr
  pure $ Game {gNumber, gSequence}

parseATurn :: T.Text -> Maybe Turn
parseATurn = mconcat . fmap parseSubturn . T.splitOn ", "

colors :: [T.Text]
colors = ["red", "green", "blue"]

parseSubturn :: T.Text -> Maybe SetOfCubes
parseSubturn sub =
  let xs = fmap (\x -> CubesNumber . read . T.unpack <$> T.stripSuffix (" " <> x) sub) colors
   in case xs of
        [r, g, b] -> Just $ SetOfCubes (fromMaybe 0 r) (fromMaybe 0 g) (fromMaybe 0 b)
        _ -> Nothing

parseGameNumber :: T.Text -> Maybe Integer
parseGameNumber str = do
  x <- T.stripPrefix "Game " str
  pure $ read $ T.unpack x

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let games = parseGames contents
      setOfCubes = SetOfCubes {socRed = 12, socGreen = 13, socBlue = 14}
  print $ sum (gNumber <$> findPossible games setOfCubes)


minimalSetFor :: Game -> SetOfCubes
minimalSetFor g = foldr merge (SetOfCubes 0 0 0) $ gSequence g
  where
    merge a b =
      SetOfCubes
        { socRed = socRed a `max` socRed b,
          socGreen = socGreen a `max` socGreen b,
          socBlue = socBlue a `max` socBlue b
        }

powerOfSet :: SetOfCubes -> Integer
powerOfSet s = fromIntegral $ socRed s * socGreen s * socBlue s

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let games = parseGames contents
      minimalSets = minimalSetFor <$> games
      powers = powerOfSet <$> minimalSets
      answer = sum powers
  print answer
