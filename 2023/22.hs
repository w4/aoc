#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (readAndParseStdin)
import Data.Bifunctor (Bifunctor (first))
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import Text.Parsec (char, digit, endBy, many1, (<|>))
import Text.Parsec.String (Parser)

main = do
  (input, _) <- first sortBricks . dropBricks <$> readAndParseStdin parser
  print $ part1 input
  print $ part2 input

part1 :: [Brick] -> Int
part1 input = length $ findNonLoadBearing input
  where
    findNonLoadBearing input = filter (flip all input . isNonLoadBearing) input
    supportedCount y = length $ filter (`isSupporting` y) input
    isNonLoadBearing x y = not (x `isSupporting` y) || supportedCount y > 1

part2 :: [Brick] -> Int
part2 input = sum $ map (snd . dropBricks . dropIndex input) [0 .. length input - 1]
  where
    dropIndex input i = take i input ++ drop (i + 1) input

-- drops bricks until they're all settled
dropBricks :: [Brick] -> ([Brick], Int)
dropBricks = foldl processBrick ([], 0)
  where
    processBrick (acc, n) x =
      let (x', n') = dropUntilSupported acc x 0
       in (x' : acc, n + min n' 1)
    dropUntilSupported acc x n =
      if isSupported acc x
        then (x, n)
        else dropUntilSupported acc (decrementZ x) (n + 1)
    isSupported acc x = any (`isSupporting` x) acc || findBaseZ x == 1
    decrementZ (Brick a b) = Brick (a {z = z a - 1}) (b {z = z b - 1})
    findBaseZ (Brick a b) = min (z a) (z b)

-- checks if a is supporting b
isSupporting :: Brick -> Brick -> Bool
isSupporting a@(Brick a1 a2) b@(Brick b1 b2) =
  let (minX1, maxX1) = (min (x a1) (x a2), max (x a1) (x a2))
      (minY1, maxY1) = (min (y a1) (y a2), max (y a1) (y a2))
      (minZ1, maxZ1) = (min (z a1) (z a2), max (z a1) (z a2))
      (minX2, maxX2) = (min (x b1) (x b2), max (x b1) (x b2))
      (minY2, maxY2) = (min (y b1) (y b2), max (y b1) (y b2))
      (minZ2, maxZ2) = (min (z b1) (z b2), max (z b1) (z b2))
      zSupport = minZ2 == (maxZ1 + 1)
      xOverlap = not (maxX1 < minX2 || maxX2 < minX1)
      yOverlap = not (maxY1 < minY2 || maxY2 < minY1)
   in a /= b && zSupport && xOverlap && yOverlap

-- sorts bricks in ascending order of Z axis
sortBricks :: [Brick] -> [Brick]
sortBricks = sortBy (\(Brick a1 a2) (Brick b1 b2) -> compare (min (z a1) (z a2)) (min (z b1) (z b2)))

parser :: Parser [Brick]
parser = sortBricks <$> parseBrick `endBy` char '\n'
  where
    parseBrick = Brick <$> parseCoord <*> (char '~' *> parseCoord)
    parseCoord = do
      x <- read <$> many1 digit <* char ','
      y <- read <$> many1 digit <* char ','
      z <- read <$> many1 digit
      return $ Coord {x, y, z}

data Coord = Coord {x :: Int, y :: Int, z :: Int} deriving (Show, Eq)

data Brick = Brick Coord Coord deriving (Show, Eq)
