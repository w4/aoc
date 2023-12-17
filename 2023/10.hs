#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Data.List
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Aoc (readAndParseStdin)

main = do
  input <- buildPipeLoop <$> readAndParseStdin parseInput
  print $ part1 input
  print $ part2 input

-- returns the first part of the loop
part1 :: [Direction] -> Int
part1 directions = fromIntegral (length directions) `div` 2

-- find the enclosed area of the loop
part2 :: [Direction] -> Int
part2 directions =
  let vertices = findVertices directions
      boundaryPoints = countBoundaryPoints vertices
   in ceiling $ picksTheorem (shoelace vertices) boundaryPoints

-- finds all the vertices for the given closed loop
findVertices :: [Direction] -> [(Int, Int)]
findVertices directions = (0, 0) : run (1, 0) (head directions) (tail directions)
  where
    run :: (Int, Int) -> Direction -> [Direction] -> [(Int, Int)]
    run coords direction [x] = [coords | direction /= x]
    run coords direction (x : xs) =
      let recurse = run (nextGridPosition coords x) x xs
       in if direction /= x then coords : recurse else recurse

-- count the boundary points
countBoundaryPoints :: [(Int, Int)] -> Int
countBoundaryPoints vertices = sum (zipWith boundaryPoints vertices (tail vertices)) - length vertices
  where
    boundaryPoints (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1) + 1

-- use pick's theorem to find the interior lattice points
picksTheorem :: Double -> Int -> Double
picksTheorem area boundaryPoints = area - (fromIntegral boundaryPoints / 2) + 1

-- calculates the area for the polygon described by the set of
-- verticies using the shoelace formula
shoelace :: [(Int, Int)] -> Double
shoelace vertices =
  let pairs = zip vertices $ tail vertices
      sumProd (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
   in fromIntegral (abs . sum $ zipWith sumProd vertices (tail vertices)) / 2

-- builds the list of directions that forms the pipe loop
buildPipeLoop :: [[Tile]] -> [Direction]
buildPipeLoop grid =
  let startingPosition = findStartingPosition grid
      initialDirection = case listToMaybe [d | d <- directions, Just (x, y, tile) <- [findNext grid startingPosition d], isValidFromDirection d tile] of
        Just o -> o
        Nothing -> error "invalid starting point, no connected points"
   in run startingPosition initialDirection
  where
    run :: (Int, Int) -> Direction -> [Direction]
    run coords direction =
      case findNext grid coords direction of
        Just (_, _, Start) -> [direction]
        Just (nextX, nextY, tile) -> direction : run (nextX, nextY) (nextDirection direction tile)
        Nothing -> error $ show (coords, direction)

-- looks for a connecting pipe in the given direction, returning Nothing if that direction
-- is invalid
findNext :: [[Tile]] -> (Int, Int) -> Direction -> Maybe (Int, Int, Tile)
findNext tiles (x, y) direction
  | isValidDirection =
      let (nextX, nextY) = nextGridPosition (x, y) direction
          nextTile = getTile tiles nextX nextY
       in Just (nextX, nextY, nextTile)
  | otherwise = Nothing
  where
    isValidDirection =
      case direction of
        North -> y > 0
        West -> x > 0
        South -> y < length tiles - 1
        East -> x < length (head tiles) - 1

-- finds the next grid position given the current coords and direction to use
nextGridPosition :: (Int, Int) -> Direction -> (Int, Int)
nextGridPosition (x, y) direction =
  let (dX, dY) = directionDelta direction
   in (x + dX, y + dY)

-- finds the starting coordinates in our input
findStartingPosition :: [[Tile]] -> (Int, Int)
findStartingPosition tiles = case listToMaybe [(x, y) | (y, row) <- zip [0 ..] tiles, (x, val) <- zip [0 ..] row, val == Start] of
  Just v -> v
  Nothing -> error "input contains no starting tile"

-- parse each input line
parseInput :: Parser [[Tile]]
parseInput = parseLine `sepBy` char '\n'

-- parse an incoming line
parseLine :: Parser [Tile]
parseLine = many1 parseTile

-- parses a single tile
parseTile :: Parser Tile
parseTile =
  choice
    [ NorthSouth <$ char '|',
      EastWest <$ char '-',
      NorthEast <$ char 'L',
      NorthWest <$ char 'J',
      SouthWest <$ char '7',
      SouthEast <$ char 'F',
      Ground <$ char '.',
      Start <$ char 'S'
    ]

data Direction = North | East | South | West deriving (Show, Enum, Eq)

-- list of all known directions on planet earth
directions :: [Direction]
directions = [North, East, South, West]

-- inverts a direction
invertDirection :: Direction -> Direction
invertDirection North = South
invertDirection South = North
invertDirection East = West
invertDirection West = East

-- maps a step in a direction to a delta on a 2d grid
directionDelta :: Direction -> (Int, Int)
directionDelta North = (0, -1)
directionDelta South = (0, 1)
directionDelta East = (1, 0)
directionDelta West = (-1, 0)

data Tile = NorthSouth | EastWest | NorthEast | NorthWest | SouthWest | SouthEast | Ground | Start deriving (Show, Enum, Eq)

-- grabs a tile at the given coordinates
getTile :: [[Tile]] -> Int -> Int -> Tile
getTile tiles x y = tiles !! y !! x

-- returns the directions the pipe connects
connectedDirections :: Tile -> Maybe (Direction, Direction)
connectedDirections tile = case tile of
  NorthSouth -> Just (North, South)
  EastWest -> Just (East, West)
  NorthEast -> Just (North, East)
  NorthWest -> Just (North, West)
  SouthWest -> Just (South, West)
  SouthEast -> Just (South, East)
  _ -> Nothing

-- checks if coming from the given direction is valid for the tile
isValidFromDirection :: Direction -> Tile -> Bool
isValidFromDirection incoming tile =
  let inverted = invertDirection incoming
   in case connectedDirections tile of
        Just (l, r) | l == inverted || r == inverted -> True
        _ -> False

-- given the incoming direction, returns the next direction the pipe turns to
nextDirection :: Direction -> Tile -> Direction
nextDirection incoming tile = case connectedDirections tile of
  Just (l, r) | l == invertDirection incoming -> r
  Just (l, r) | r == invertDirection incoming -> l
  _ -> error $ show (incoming, tile)
