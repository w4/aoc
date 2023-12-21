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

-- parse an incoming grow
parseGridRow :: Parser [Bool]
parseGridRow = many1 parseGridTile

-- parses a single tile on a grid
parseGridTile :: Parser Bool
parseGridTile =
  choice
    [ True <$ char '#',
      False <$ char '.'
    ]
