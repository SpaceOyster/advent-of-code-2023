{-# LANGUAGE OverloadedStrings #-}

module Day.Three (solution1, solution2) where

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
    Just i -> stepXForwardBy i >> parseOnCurrentPosition line >> parseLine line
    Nothing -> pure ()

parseOnCurrentPosition :: T.Text -> NoteParser ()
parseOnCurrentPosition line' = do
  x <- St.gets (cX . psCoordinates) -- gets current position in line
  let line = T.drop x line' -- skips to current position
  fromMaybe stepXForward $
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

newtype Gear = Gear {gCoordinates :: Coordinates}
  deriving (Show)

findGears :: Note -> [Gear]
findGears n = Gear <$> (Map.keys $ Map.filter (== Symbol '*') $ nSymbols n)

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
