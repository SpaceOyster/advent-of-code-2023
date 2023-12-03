{-# LANGUAGE OverloadedStrings #-}

module Day.Three (solution1, solution2) where

{-
--- Day 3: Gear Ratios ---

You and the Elf eventually reach a gondola lift station; he says the gondola
lift will take you up to the water source, but this is as far as he can bring
you. You go inside.

It doesn't take long to find the gondolas, but there seems to be a problem:
they're not moving.

"Aaah!"

You turn around to see a slightly-greasy Elf with a wrench and a look of
surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working
right now; it'll still be a while before I can fix it." You offer to help.

The engineer explains that an engine part seems to be missing from the engine,
but nobody can figure out which one. If you can add up all the part numbers in
the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation
of the engine. There are lots of numbers and symbols you don't really
understand, but apparently any number adjacent to a symbol, even diagonally,
is a "part number" and should be included in your sum. (Periods (.) do not
count as a symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not
adjacent to a symbol: 114 (top right) and 58 (middle right). Every other
number is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum of all
of the part numbers in the engine schematic?
-}

import Control.Applicative (asum)
import Control.Monad.Trans.State as St
import qualified Data.Char as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf (IsChar)

data Coordinates = Coordinates {cX :: Int, cY :: Int}
  deriving (Show, Eq, Ord)

nullifyX :: Coordinates -> Coordinates
nullifyX c = c {cX = 0}

incX :: Int -> Coordinates -> Coordinates
incX x c = c {cX = cX c + x}

incY :: Int -> Coordinates -> Coordinates
incY y c = c {cY = cY c + y}

newtype Symbol = Symbol {getSymbol :: Char}
  deriving (Show, Eq, Ord, IsChar)

newtype PartNumber = PartNumber
  { pnNumber :: Integer
  }
  deriving (Show, Eq, Ord, Num, Enum, Real)

pnLength :: PartNumber -> Int
pnLength = length . show . pnNumber

data Note = Note
  { nNumbers :: Map.Map Coordinates PartNumber,
    nSymbols :: Map.Map Coordinates Symbol
  }
  deriving (Show)

instance Semigroup Note where
  a <> b =
    Note
      { nNumbers = nNumbers a <> nNumbers b,
        nSymbols = nSymbols a <> nSymbols b
      }

addToSymbols :: Coordinates -> Symbol -> Note -> Note
addToSymbols k v n = n {nSymbols = Map.insert k v $ nSymbols n}

addToNumbers :: Coordinates -> PartNumber -> Note -> Note
addToNumbers k v n = n {nNumbers = Map.insert k v $ nNumbers n}

instance Monoid Note where
  mempty = Note {nNumbers = mempty, nSymbols = mempty}

data ParserState = ParserState
  { psCoordinates :: Coordinates,
    psResult :: Note
  }
  deriving (Show)

emptyParserState :: ParserState
emptyParserState = ParserState {psCoordinates = Coordinates 0 0, psResult = mempty}

type NoteParser = St.State ParserState

modifyCoordinates :: (Coordinates -> Coordinates) -> ParserState -> ParserState
modifyCoordinates f s = s {psCoordinates = f $ psCoordinates s}

modifyNote :: (Note -> Note) -> ParserState -> ParserState
modifyNote f s = s {psResult = f $ psResult s}

stepXForwardBy :: Int -> NoteParser ()
stepXForwardBy = modify . modifyCoordinates . incX

stepYForwardBy :: Int -> NoteParser ()
stepYForwardBy = modify . modifyCoordinates . incY

stepXForward :: NoteParser ()
stepXForward = stepXForwardBy 1

stepYForward :: NoteParser ()
stepYForward = stepYForwardBy 1

goToBegingOfLine :: NoteParser ()
goToBegingOfLine = modify $ modifyCoordinates nullifyX

addSymbolToState :: Symbol -> NoteParser ()
addSymbolToState s = do
  cs <- gets psCoordinates
  modify $ modifyNote (addToSymbols cs s)

addNumberToState :: PartNumber -> NoteParser ()
addNumberToState s = do
  cs <- gets psCoordinates
  modify $ modifyNote (addToNumbers cs s)

parseNote :: T.Text -> Note
parseNote text = flip evalState emptyParserState $ go (T.lines text)
  where
    go [] = gets psResult
    go (x : xs) = parseLine x >> goToBegingOfLine >> stepYForward >> go xs

parseLine :: T.Text -> NoteParser ()
parseLine line = do
  x <- St.gets (cX . psCoordinates) -- gets current position in line
  case T.findIndex (/= '.') $ T.drop x line of -- gets token index relatively to x
    Just i -> stepXForwardBy i >> parseToken line >> parseLine line
    Nothing -> pure ()

parseToken :: T.Text -> NoteParser ()
parseToken line' = do
  x <- St.gets (cX . psCoordinates) -- gets current position in line
  let line = T.drop x line' -- skips to current position
  fromMaybe stepXForward $ -- run the one that succedes or just stepXForward
    asum
      [ (\s -> addSymbolToState s >> stepXForward) <$> parseSymbol line,
        (\n -> addNumberToState n >> stepXForwardBy (pnLength n)) <$> parseNumber line
      ]

parseNumber :: T.Text -> Maybe PartNumber
parseNumber line = case T.takeWhile C.isDigit line of
  "" -> Nothing
  x -> Just $ PartNumber {pnNumber = read $ T.unpack x}

parseSymbol :: T.Text -> Maybe Symbol
parseSymbol line =
  let x = T.head line
   in if x /= '.' && not (C.isDigit x) then Just $ Symbol x else Nothing

-- Set is an unordered container, that holds only unique values. It's used here
-- to automatically iliminate duplicates. Could use a List with `nub` in the end
coordinatesToCheck :: Coordinates -> PartNumber -> Set.Set Coordinates
coordinatesToCheck c p = coords
  where
    pLength = pnLength p
    util = [-1, 0 .. pLength]
    f x = [incX x (incY (-1) c), incX x c, incX x (incY 1 c)]
    coords = Set.fromList (util >>= f) `Set.difference` Set.fromList (take pLength $ iterate (incX 1) c)

isNumberValid :: Note -> Coordinates -> PartNumber -> Bool
isNumberValid n c p = foldr (\a b -> check a || b) False toCheck
  where
    toCheck = coordinatesToCheck c p
    check c' = Map.member c' $ nSymbols n

findValidNumbers :: Note -> [PartNumber]
findValidNumbers n =
  Map.elems $ Map.filterWithKey (isNumberValid n) (nNumbers n)

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  print $ sum $ findValidNumbers $ parseNote contents

{-
--- Part Two ---

The engineer finds the missing part and installs it in the engine! As the
engine springs to life, you jump in the closest gondola, finally ready to
ascend to the water source.

You don't seem to be going very fast, though. Maybe something is still wrong?
Fortunately, the gondola has a phone labeled "help", so you pick it up and
the engineer answers.

Before you can explain the situation, she suggests that you look out the window
. There stands the engineer, holding a phone in one hand and waving with the
other. You're going so slowly that you haven't even left the station. You exit
the gondola.

The missing part wasn't the only issue - one of the gears in the engine is
wrong. A gear is any * symbol that is adjacent to exactly two part numbers.
  Its gear ratio is the result of multiplying those two numbers together.

This time, you need to find the gear ratio of every gear and add them all up
so that the engineer can figure out which gear needs to be replaced.

Consider the same engine schematic again:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, there are two gears. The first is in the top left; it has
part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the
lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear
because it is only adjacent to one part number.) Adding up all of the gear
ratios produces 467835.

What is the sum of all of the gear ratios in your engine schematic?
-}

newtype Gear = Gear {gCoordinates :: Coordinates}
  deriving (Show)

findGears :: Note -> [Gear]
findGears n = Gear <$> Map.keys (Map.filter (== Symbol '*') $ nSymbols n)

isNumberAdjacentTo :: PartNumber -> Coordinates -> Gear -> Bool
isNumberAdjacentTo p pnC g = Set.member (gCoordinates g) $ coordinatesToCheck pnC p

findGearAdjacentNumbers :: Note -> Gear -> [PartNumber]
findGearAdjacentNumbers n g = Map.elems $ Map.filterWithKey f $ nNumbers n
  where
    gY = cY $ gCoordinates g
    f c p = cY c `elem` [gY - 1, gY, gY + 1] && isNumberAdjacentTo p c g

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let note = parseNote contents
      gearNumbers = findGearAdjacentNumbers note <$> findGears note
      onlyPairs = filter (\x -> length x == 2) gearNumbers
      gearRatios = foldr (\a b -> b * pnNumber a) 1 <$> onlyPairs
  print $ sum gearRatios
