#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (parseMultiChoiceGrid, readAndParseStdin)
import Data.List (findIndices, transpose)
import Data.Maybe (isJust, isNothing)

main = do
  input <- readAndParseStdin parseMultiChoiceGrid
  print $ part1 input
  print $ part2 input

part1 :: [[Maybe Bool]] -> Int
part1 = countScore . tiltNorth

part2 :: [[Maybe Bool]] -> Int
part2 = countScore . spinCycle 1000
  where
    spinCycle 0 acc = acc
    spinCycle n acc = spinCycle (n - 1) ((tiltEast . tiltSouth . tiltWest . tiltNorth) acc)

countScore :: [[Maybe Bool]] -> Int
countScore [] = 0
countScore (x : xs) = length (filter (== Just True) x) * (length xs + 1) + countScore xs

tiltNorth :: [[Maybe Bool]] -> [[Maybe Bool]]
tiltNorth = transpose . map (move [] 0) . transpose
  where
    move :: [Maybe Bool] -> Int -> [Maybe Bool] -> [Maybe Bool]
    move acc _ [] = acc
    move acc lastSeenJust (x : xs)
      | x == Just True = move (take lastSeenJust acc ++ x : drop lastSeenJust acc) (lastSeenJust + 1) xs
      | x == Just False = move (acc ++ [x]) (length acc + 1) xs
      | isNothing x = move (acc ++ [x]) lastSeenJust xs

tiltWest :: [[Maybe Bool]] -> [[Maybe Bool]]
tiltWest = transpose . tiltNorth . transpose

tiltEast :: [[Maybe Bool]] -> [[Maybe Bool]]
tiltEast = transpose . reverse . tiltNorth . reverse . transpose

tiltSouth :: [[Maybe Bool]] -> [[Maybe Bool]]
tiltSouth = reverse . tiltNorth . reverse
