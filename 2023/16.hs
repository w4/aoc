#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (readAndParseStdin)
import qualified Data.Set as Set
import Text.Parsec (char, choice, many1, sepBy)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parseInput
  print $ part1 input
  print $ part2 input

part1 :: [[GridTile]] -> Int
part1 input = length $ traverseWithSeen input (0, 0, R)

part2 :: [[GridTile]] -> Int
part2 input =
  let maxY = length input - 1
      maxX = length (head input) - 1
      allStartingPositions = map (,0) [0 .. maxX] ++ map (0,) [0 .. maxY] ++ map (,maxY) [0 .. maxX] ++ map (maxX,) [0 .. maxY]
      allStartingPositionsWithHeadings = [(x, y, h) | (x, y) <- allStartingPositions, h <- allDirections]
   in maximum $ map (length . traverseWithSeen input) allStartingPositionsWithHeadings

traverseWithSeen :: [[GridTile]] -> (Int, Int, Heading) -> Set.Set (Int, Int)
traverseWithSeen grid (x, y, heading) = Set.map (\(x, y, heading) -> (x, y)) $ go (x, y) heading Set.empty
  where
    go :: (Int, Int) -> Heading -> Set.Set (Int, Int, Heading) -> Set.Set (Int, Int, Heading)
    go (x, y) heading acc =
      let newSet = Set.insert (x, y, heading) acc
       in if Set.member (x, y, heading) acc
            then acc
            else case getTile grid (x, y) of
              Just VertSplitter | isHorizontal heading -> go (nextPos (x, y) Up) Up $ go (nextPos (x, y) Down) Down newSet
              Just HorizSplitter | isVertical heading -> go (nextPos (x, y) L) L $ go (nextPos (x, y) R) R newSet
              Just v | v == RightSlantMirror || v == LeftSlantMirror -> go (nextPos (x, y) $ getNextDirection v heading) (getNextDirection v heading) newSet
              Just _ -> go (nextPos (x, y) heading) heading newSet
              Nothing -> acc

parseInput :: Parser [[GridTile]]
parseInput = many1 parseGridTile `sepBy` char '\n'

parseGridTile :: Parser GridTile
parseGridTile =
  choice
    [ Empty <$ char '.',
      RightSlantMirror <$ char '/',
      LeftSlantMirror <$ char '\\',
      VertSplitter <$ char '|',
      HorizSplitter <$ char '-'
    ]

data GridTile = Empty | RightSlantMirror | LeftSlantMirror | VertSplitter | HorizSplitter deriving (Enum, Show, Eq)

data Heading = Up | Down | L | R deriving (Enum, Show, Ord, Eq)

allDirections = [Up, Down, L, R]

getNextDirection :: GridTile -> Heading -> Heading
getNextDirection RightSlantMirror Up = R
getNextDirection RightSlantMirror Down = L
getNextDirection RightSlantMirror R = Up
getNextDirection RightSlantMirror L = Down
getNextDirection LeftSlantMirror Up = L
getNextDirection LeftSlantMirror Down = R
getNextDirection LeftSlantMirror R = Down
getNextDirection LeftSlantMirror L = Up

isHorizontal :: Heading -> Bool
isHorizontal L = True
isHorizontal R = True
isHorizontal _ = False

isVertical :: Heading -> Bool
isVertical = not . isHorizontal

getTile :: [[GridTile]] -> (Int, Int) -> Maybe GridTile
getTile g (x, y)
  | y >= length g || y < 0 = Nothing
  | x >= length (g !! y) || x < 0 = Nothing
  | otherwise = Just $ (g !! y) !! x

nextPos :: (Int, Int) -> Heading -> (Int, Int)
nextPos (x, y) Up = (x, y - 1)
nextPos (x, y) Down = (x, y + 1)
nextPos (x, y) L = (x - 1, y)
nextPos (x, y) R = (x + 1, y)
