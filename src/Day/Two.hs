{-# LANGUAGE OverloadedStrings #-}

module Day.Two (solution1, solution2) where

import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.IO as T

{-
--- Day 2: Cube Conundrum ---

You're launched high into the atmosphere! The apex of your trajectory just
barely reaches the surface of a large island floating in the sky. You gently
land in a fluffy pile of leaves. It's quite cold, but you don't see much snow.
  An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for the
lack of snow. He'll be happy to explain the situation, but it's a bit of a walk
  , so you have some time. They don't get many visitors up here; would you
  like to play a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are either red
  , green, or blue. Each time you play this game, he will hide a secret number
  of cubes of each color in the bag, and your goal is to figure out
  information about the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf will reach
into the bag, grab a handful of random cubes, show them to you, and then put
them back in the bag. He'll do this a few times per game.

You play several games and record the information from each game (your puzzle
input). Each game is listed with its ID number (like the 11 in Game 11: ...)
followed by a semicolon-separated list of subsets of cubes that were revealed
from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, three sets of cubes are revealed from the bag (and then put back
again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red
cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag
contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had
been loaded with that configuration. However, game 3 would have been
impossible because at one point the Elf showed you 20 red cubes at once;
similarly, game 4 would also have been impossible because the Elf showed you
15 blue cubes at once. If you add up the IDs of the games that would have been
possible, you get 8.

Determine which games would have been possible if the bag had been loaded with
only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the
IDs of those games?
-}

newtype CubesNumber = CubesNumber {getCubesNumber :: Integer}
  deriving (Num, Eq, Ord, Enum, Show, Real)

instance Integral CubesNumber where
  toInteger = getCubesNumber
  quotRem a b =
    let (x, y) = quotRem (getCubesNumber a) (getCubesNumber b)
     in (CubesNumber x, CubesNumber y)

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
      answer = sum (gNumber <$> findPossible games setOfCubes)
  print answer

{-
--- Part Two ---

The Elf says they've stopped producing snow because they aren't getting any
water! He isn't sure why the water stopped; however, he can show you how to
get to the water source to check it out for yourself. It's just up ahead!

As you continue your walk, the Elf poses a second question: in each game you
played, what is the fewest number of cubes of each color that could have been
in the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

- In game 1, the game could have been played with as few as 4 red, 2 green,
  and 6 blue cubes. If any color had even one fewer cube, the game would
  have been impossible.
- Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
- Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
- Game 4 required at least 14 red, 3 green, and 15 blue cubes.
- Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

The power of a set of cubes is equal to the numbers of red, green, and blue
cubes multiplied together. The power of the minimum set of cubes in game 1 is
48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these
five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been present. What
is the sum of the power of these sets?
-}

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
