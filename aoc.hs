module Aoc where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

-- read and parse stdin
readAndParseStdin :: Parser a -> IO a
readAndParseStdin parser = do
  content <- getContents
  case parse parser "" content of
    Left parseError -> error $ show parseError
    Right doc -> return doc

-- parse multiple grids
parseMultipleGrids :: Parser [[[Bool]]]
parseMultipleGrids = parseGrid `sepBy` string "\n"

-- parse an entire grid
parseGrid :: Parser [[Bool]]
parseGrid = parseGridRow `endBy` char '\n'

-- parse an incoming row
parseGridRow :: Parser [Bool]
parseGridRow = many1 parseGridTile

-- parses a single tile on a grid
parseGridTile :: Parser Bool
parseGridTile =
  choice
    [ True <$ char '#',
      False <$ char '.'
    ]

-- parse an entire multichoice grid
parseMultiChoiceGrid :: Parser [[Maybe Bool]]
parseMultiChoiceGrid = parseMultiChoiceGridRow `endBy` char '\n'

-- parse an incoming row
parseMultiChoiceGridRow :: Parser [Maybe Bool]
parseMultiChoiceGridRow = many1 parseMultiChoiceGridTile

-- parse a single multi-choice tile on a grid
parseMultiChoiceGridTile :: Parser (Maybe Bool)
parseMultiChoiceGridTile =
  choice
    [ Just True <$ char 'O',
      Just False <$ char '#',
      Nothing <$ char '.'
    ]

-- debugging for multi-choice tile grids
printMultiChoiceGrid :: [[Maybe Bool]] -> IO ()
printMultiChoiceGrid xs = putStrLn $ unlines $ (map . map) multiChoiceGridTileChar xs
  where
    multiChoiceGridTileChar (Just True) = 'O'
    multiChoiceGridTileChar (Just False) = '#'
    multiChoiceGridTileChar Nothing = '.'

-- calculates the area for a polygon described by the set of
-- verticies using the shoelace formula
shoelace :: [(Int, Int)] -> Double
shoelace vertices =
  let pairs = zip vertices $ tail vertices
      sumProd (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
   in fromIntegral (abs . sum $ zipWith sumProd vertices (tail vertices)) / 2

