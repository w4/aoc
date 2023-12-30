#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ parallel ])"

import Aoc (readAndParseStdin)
import Control.Parallel (par, pseq)
import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Text.Parsec (char, choice, endBy, many1)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parseGrid
  print $ part1 input
  print $ part2 input

part1 :: [[Path]] -> Int
part1 = walkMazeH True

part2 :: [[Path]] -> Int
part2 = walkMazeH False

walkMazeH :: Bool -> [[Path]] -> Int
walkMazeH slippery input = maximum $ map (subtract 1 . IntSet.size) $ fromDList $ walkMaze (1, 0) (IntSet.singleton $ hashCoord (1, 0))
  where
    maxX = length (head input)
    maxY = length input
    exit = (maxX - 2, maxY - 1)
    grid = array ((0, 0), (maxX, maxY)) [((x, y), input !! y !! x) | y <- [0 .. maxY - 1], x <- [0 .. maxX - 1]]
    hashCoord (x, y) = x * 1000 + y
    walkMaze :: (Int, Int) -> IntSet -> DList IntSet
    walkMaze pos@(x, y) cacc
      | pos == exit = toDList [newCacc]
      | slippery = case grid ! (x, y) of
          Path -> parallelTraverseAll
          Forest -> error "invalid state"
          UpSlope -> parallelTraverse (x, y - 1)
          RightSlope -> parallelTraverse (x + 1, y)
          DownSlope -> parallelTraverse (x, y + 1)
          LeftSlope -> parallelTraverse (x - 1, y)
      | otherwise = parallelTraverseAll
      where
        newCacc = IntSet.insert (hashCoord pos) cacc
        isValidPath pos@(x, y) = y >= 0 && x >= 0 && y < maxY && x < maxX && IntSet.notMember (hashCoord pos) cacc && (grid ! pos) /= Forest
        parallelTraverseAll = foldr parallelWalk (toDList []) $ filter isValidPath [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
        parallelWalk path acc = go path `par` (acc `pseq` (acc `dappend` go path))
        parallelTraverse pos = if isValidPath pos then go pos else toDList []
        go pos = walkMaze pos newCacc

type DList a = [a] -> [a]

{-# INLINEABLE toDList #-}
toDList :: [a] -> DList a
toDList xs = (xs ++)

{-# INLINEABLE fromDList #-}
fromDList :: DList a -> [a]
fromDList dl = dl []

{-# INLINEABLE dappend #-}
dappend :: DList a -> DList a -> DList a
dappend dl1 dl2 = dl1 . dl2

parseGrid :: Parser [[Path]]
parseGrid = parseGridRow `endBy` char '\n'
  where
    parseGridRow = many1 parseGridTile
    parseGridTile =
      choice
        [ Path <$ char '.',
          Forest <$ char '#',
          UpSlope <$ char '^',
          RightSlope <$ char '>',
          DownSlope <$ char 'v',
          LeftSlope <$ char '<'
        ]

data Path = Path | Forest | UpSlope | RightSlope | DownSlope | LeftSlope deriving (Enum, Show, Eq)
