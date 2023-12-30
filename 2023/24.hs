#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ hmatrix ])"
import Aoc (readAndParseStdin)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Numeric.LinearAlgebra
import Text.Parsec (char, digit, many1, spaces, (<|>), sepBy, string)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parse
  print $ part1 input
  print $ part2 input

part1 :: [Object] -> Int
part1 xs = length . filter intersects $ [(x, y) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i < j]
  where
    minV = 200000000000000
    maxV = 400000000000000
    intersects (a, b) =
      let (dx1, dy1) = vec velocity a
          (dx2, dy2) = vec velocity b
          (px1, py1) = vec pos a
          (px2, py2) = vec pos b
          tA = timeFactorA dx1 dy1 px1 py1 dx2 dy2 px2 py2
          tB = timeFactorB px1 dx1 tA px2 dx2
          (ix, iy) = intersectionPoint px1 py1 tB dx1 dy1
       in tA >= 0 && tB >= 0 && ix >= minV && ix <= maxV && iy >= minV && iy <= maxV
    vec :: (Object -> V3) -> Object -> (Double, Double)
    vec f obj = (fromIntegral $ x . f $ obj, fromIntegral $ y . f $ obj)
    timeFactorA dx1 dy1 px1 py1 dx2 dy2 px2 py2 = (dx1 * (py1 - py2) + dy1 * (px2 - px1)) / (dx1 * dy2 - dx2 * dy1)
    timeFactorB px1 dx1 tA px2 dx2 = (px2 + tA * dx2 - px1) / dx1
    intersectionPoint px1 py1 tB dx1 dy1 = (px1 + tB * dx1, py1 + tB * dy1)

part2 :: [Object] -> Int
part2 os =
  let [o0, o1, o2] = take 3 os
      extract v p o = (fromV3 $ v o, fromV3 $ p o)
      (v0, p0) = extract velocity pos o0
      (v1, p1) = extract velocity pos o1
      (v2, p2) = extract velocity pos o2
      (m1, m2) = crossDiff v0 p0 v1 p1
      (m3, m4) = crossDiff v0 p0 v2 p2
      m = fromBlocks [[m1, m2], [m3, m4]]
      rhs = asColumn $ vjoin [cross p0 v0 - cross p1 v1, cross p0 v0 - cross p2 v2]
      result = fromJust $ linearSolve m rhs
   in round $ abs $ sumElements $ subVector 0 3 $ head $ toColumns result

fromV3 :: V3 -> Vector Double
fromV3 (V3 x y z) = fromList [fromIntegral x, fromIntegral y, fromIntegral z]

{- ORMOLU_DISABLE -}
crossDiff :: Vector Double -> Vector Double -> Vector Double -> Vector Double -> (Matrix Double, Matrix Double)
crossDiff v p v' p' = (crossMatrix v - crossMatrix v', -crossMatrix p + crossMatrix p')
  where
    crossMatrix :: Vector Double -> Matrix Double
    crossMatrix v =
      (3 >< 3)
        [ 0,      -v ! 2,  v ! 1,
          v ! 2,   0,     -v ! 0,
          -v ! 1,  v ! 0,  0 ]
{- ORMOLU_ENABLE -}

parse :: Parser [Object]
parse = parseLine `sepBy` char '\n'
  where
    parseLine = do
      pos <- parseV3 <* spaces <* char '@' <* spaces
      velocity <- parseV3
      return Object {pos, velocity}
    parseV3 = do
      x <- parseNumber <* char ',' <* spaces
      y <- parseNumber <* char ',' <* spaces
      V3 x y <$> parseNumber
    parseNumber = read <$> many1 (digit <|> char '-')

data Object = Object {pos :: V3, velocity :: V3} deriving (Show)

data V3 = V3 Int Int Int deriving (Show)

x :: V3 -> Int
x (V3 x _ _) = x

y :: V3 -> Int
y (V3 _ y _) = y

z :: V3 -> Int
z (V3 _ _ z) = z
