#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"
import Data.Char (isNumber)
import Data.List (findIndices)

{- https://adventofcode.com/2023/day/3 -}

main = do
  input <- getContents
  let (schematics, symbols) = takeSymbols input
  print $ sum $ part1FindNumbersAdjacentToSchematic schematics symbols
  print $ sum $ map product $ part2FindGearPartNumbers schematics symbols

-- find all the part numbers (schematics that are adjacent to a symbol)
part1FindNumbersAdjacentToSchematic :: [Schematic] -> [Symbol] -> [Int]
part1FindNumbersAdjacentToSchematic schematics symbols = map partNumber $ filter (isSchematicAdjacentToAnySymbol symbols) schematics

-- find all part numbers for gears (schematics that are adjacent to exactly two symbols)
part2FindGearPartNumbers :: [Schematic] -> [Symbol] -> [[Int]]
part2FindGearPartNumbers schematics symbols = map (map partNumber) $ filter (\inner -> length inner == 2) $ map (findSchematicsAdjacentToSymbol schematics) symbols

-- returns all schematics that are adjacent to the given symbol
findSchematicsAdjacentToSymbol :: [Schematic] -> Symbol -> [Schematic]
findSchematicsAdjacentToSymbol schematics symbol = filter (`isSchematicAdjacent` symbol) schematics

-- returns true, if the given schematic is adjacent to any of the given symbols
isSchematicAdjacentToAnySymbol :: [Symbol] -> Schematic -> Bool
isSchematicAdjacentToAnySymbol symbol schematic = any (isSchematicAdjacent schematic) symbol

-- returns true, if the given schematic is directly adjacent to the given symbol
isSchematicAdjacent :: Schematic -> Symbol -> Bool
isSchematicAdjacent sc sy = isAdjacent (symbolCoords sy) (schematicCoords sc)

-- returns true, grid position (px, py) is adjacent to single height multi column grid position (rx, ry, width)
isAdjacent :: (Int, Int) -> (Int, Int, Int) -> Bool
isAdjacent (px, py) (rx, ry, width) =
  let leftX = rx
      rightX = rx + width - 1
      upperY = ry + 1
      lowerY = ry - 1
   in (px >= leftX - 1 && px <= rightX + 1 && py == ry) -- adjacent horizontally
        || (py == upperY || py == lowerY) && (px >= leftX && px <= rightX) -- adjacent vertically
        || (px == leftX - 1 || px == rightX + 1) && (py == upperY || py == lowerY) -- adjacent diagonally

data Schematic = Schematic
  { partNumber :: Int,
    schematicCoords :: (Int, Int, Int)
  }
  deriving (Show)

data Symbol = Symbol
  { symbolCoords :: (Int, Int),
    symbolType :: Char
  }
  deriving (Show)

-- parse the entire input
takeSymbols :: String -> ([Schematic], [Symbol])
takeSymbols input = takeSymbol input Nothing (0, 0) [] []

-- recursively parses the input character by character
takeSymbol :: String -> Maybe Schematic -> (Int, Int) -> [Schematic] -> [Symbol] -> ([Schematic], [Symbol])
takeSymbol "" Nothing _ schematicAcc symbolAcc = (schematicAcc, symbolAcc)
takeSymbol "" (Just inProgressSchematic) _ schematicAcc symbolAcc = (schematicAcc ++ [inProgressSchematic], symbolAcc)
takeSymbol (x : xs) inProgressSchematic (posX, posY) schematicAcc symbolAcc =
  case x of
    _ | isNumber x -> takeSymbol xs (Just $ appendToSchematic inProgressSchematic (posX, posY) (read [x])) (posX + 1, posY) schematicAcc symbolAcc
    '.' -> takeSymbol xs Nothing (posX + 1, posY) (maybeAppend schematicAcc inProgressSchematic) symbolAcc
    '\n' -> takeSymbol xs Nothing (0, posY + 1) (maybeAppend schematicAcc inProgressSchematic) symbolAcc
    _ -> takeSymbol xs Nothing (posX + 1, posY) (maybeAppend schematicAcc inProgressSchematic) (symbolAcc ++ [buildSymbol posX posY x])

-- appends a character to the schematic, creating a new schematic if one isn't already instantiated
appendToSchematic :: Maybe Schematic -> (Int, Int) -> Int -> Schematic
appendToSchematic (Just schematic) _ c =
  let (x, y, n) = schematicCoords schematic
      currPartNumber = partNumber schematic
   in schematic {partNumber = currPartNumber * 10 + c, schematicCoords = (x, y, n + 1)}
appendToSchematic Nothing (x, y) c = Schematic {partNumber = c, schematicCoords = (x, y, 1)}

-- append a Maybe Schematic to [Schematic]
maybeAppend :: [Schematic] -> Maybe Schematic -> [Schematic]
maybeAppend out (Just new) = out ++ [new]
maybeAppend out Nothing = out

-- easy constructor for a Symbol
buildSymbol :: Int -> Int -> Char -> Symbol
buildSymbol x y symbolType = Symbol {symbolCoords = (x, y), symbolType}
