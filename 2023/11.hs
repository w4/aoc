#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (parseGrid, readAndParseStdin)
import Data.List (tails, transpose)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parseGrid
  let expansion = findEmptySpace input
  print $ part1 input expansion
  print $ part2 input expansion

-- part 1, expansion rate of 1
part1 :: [[Bool]] -> ([Int], [Int]) -> Int
part1 = calculatePaths 1

-- part2, expansion rate of 999999
part2 :: [[Bool]] -> ([Int], [Int]) -> Int
part2 = calculatePaths 999999

-- given the expansion multiplier, input and expansion points - find all the galaxies
-- in the input and calculate the shortest paths between them, applying the expansion
-- multiplier between galaxies with empty space between them.
calculatePaths :: Int -> [[Bool]] -> ([Int], [Int]) -> Int
calculatePaths multiplier input expansion = sum $ map (shortestPathDistance multiplier expansion) $ pairs (findGalaxies input)

-- finds the shortest direct path between two coordinates
shortestPathDistance :: Int -> ([Int], [Int]) -> ((Int, Int), (Int, Int)) -> Int
shortestPathDistance spaceMultiplier expansion ((x1, y1), (x2, y2)) =
  let space = spaceMultiplier * crossesExpansionCount expansion (x1, y1) (x2, y2)
   in abs (x2 - x1) + abs (y2 - y1) + space

-- find the x,y coordinates of all galaxies
findGalaxies :: [[Bool]] -> [(Int, Int)]
findGalaxies xs = concat [[(x, y) | (x, val) <- zip [0 ..] xs, val] | (y, xs) <- zip [0 ..] xs]

-- counts the amount of times a direct path between x1,y1 and x2,y2 would cross an expansion of space
crossesExpansionCount :: ([Int], [Int]) -> (Int, Int) -> (Int, Int) -> Int
crossesExpansionCount (xexp, yexp) (x1, y1) (x2, y2) =
  countCrosses x1 x2 xexp + countCrosses y1 y2 yexp
  where
    countCrosses c1 c2 = length . filter (crosses c1 c2)
    crosses c1 c2 exp = (c1 - exp) * (c2 - exp) < 0

-- returns the coordinates of empty space on the x and y coordinates
findEmptySpace :: [[Bool]] -> ([Int], [Int])
findEmptySpace grid = (findX grid, findY grid)
  where
    findEmpty [] = []
    findEmpty ((idx, x) : xs) = if or x then findEmpty xs else idx : findEmpty xs
    findY = findEmpty . zip [0 ..]
    findX = findY . transpose

-- returns combinations of [a]
pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
