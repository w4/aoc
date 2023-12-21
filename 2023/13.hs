#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (parseMultipleGrids, readAndParseStdin)
import Data.Bifunctor (Bifunctor (first))
import Data.List (transpose)

main = do
  input <- readAndParseStdin parseMultipleGrids
  print $ part1 input
  print $ part2 input

part1 :: [[[Bool]]] -> Int
part1 = sum . map (findReflectionAny 0)

part2 :: [[[Bool]]] -> Int
part2 = sum . map (findReflectionAny 1)

findReflectionAny :: Int -> [[Bool]] -> Int
findReflectionAny diffs xs =
  let hl = findReflection diffs xs
      hr = findReflection diffs . reverse $ xs
      vl = findReflection diffs . transpose $ xs
      vr = findReflection diffs . reverse . transpose $ xs
   in case (hl, hr, vl, vr) of
        (v, 0, 0, 0) -> v * 100
        (0, v, 0, 0) -> (length xs - v) * 100
        (0, 0, v, 0) -> v
        (0, 0, 0, v) -> (length . head) xs - v

findReflection :: Int -> [[Bool]] -> Int
findReflection diffs xs = go xs
  where
    go [] = 0
    go xs
      | cmp (first reverse $ splitAt (length xs `div` 2) xs) == diffs = length xs `div` 2
      | otherwise = go (init xs)
    cmp (x1, x2) = sum $ zipWith cmpInner x1 x2
    cmpInner x1 x2 = length . filter id $ zipWith (/=) x1 x2
