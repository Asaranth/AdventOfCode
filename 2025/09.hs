-- | Day 09: Movie Theater
--
-- Calculates maximal rectangle areas from red perimeter coordinates and performs boundary intersection checks.
module Main where

import Data.List (tails)
import Utils (getInputData)

-- | 2D coordinate point represented by (x, y).
type Point = (Int, Int)

-- | Line segment bounded by two 2D points.
type Line = (Point, Point)

-- | Parses a comma-separated coordinate pair string into a 'Point'.
parsePoint :: String -> Point
parsePoint s = let (a, b) = span (/= ',') s in (read a, read (tail b))

-- | Calculates the area of the axis-aligned bounding box formed by two opposite diagonal corners.
areaIfRectangle :: (Point, Point) -> Int
areaIfRectangle ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = 0
  | otherwise = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

-- | Computes the areas of all pairwise rectangles formed by the input points.
calculateAreas :: [Point] -> [Int]
calculateAreas pts = [areaIfRectangle (p1, p2) | (p1 : rest) <- tails pts, p2 <- rest]

-- | Constructs consecutive polygon boundary segments from ordered vertices.
buildLines :: [Point] -> [Line]
buildLines pts = zip pts (tail $ cycle pts)

-- | Tests whether a line segment strictly intersects the interior of a rectangle.
intersects :: (Int, Int, Int, Int) -> Line -> Bool
intersects (rx1, ry1, rx2, ry2) ((x1, y1), (x2, y2))
  | x1 == x2 =
      let x = x1
          minY = min y1 y2
          maxY = max y1 y2
       in x > rx1 && x < rx2 && maxY > ry1 && minY < ry2
  | y1 == y2 =
      let y = y1
          minX = min x1 x2
          maxX = max x1 x2
       in y > ry1 && y < ry2 && maxX > rx1 && minX < rx2
  | otherwise = error "Lines must be horizontal or vertical"

-- | Constructs an axis-aligned bounding box @(minX, minY, maxX, maxY)@ from two corner points.
rectangle :: Point -> Point -> (Int, Int, Int, Int)
rectangle (x1, y1) (x2, y2) = (min x1 x2, min y1 y2, max x1 x2, max y1 y2)

-- | Computes the inclusive area of a bounding box.
area :: (Int, Int, Int, Int) -> Int
area (x1, y1, x2, y2) = (x2 - x1 + 1) * (y2 - y1 + 1)

-- | Solves Part One: returns the maximum bounding rectangle area among all point pairs.
part1 :: [String] -> Int
part1 = maximum . calculateAreas . map parsePoint

-- | Solves Part Two: returns the largest rectangle area that does not intersect any polygon boundary edges.
part2 :: [String] -> Int
part2 ls =
  let redPoints = map parsePoint ls
      linesList = buildLines redPoints
      pairs = [(p1, p2) | (p1 : rest) <- tails redPoints, p2 <- rest]
      areas = [area rect | (p1, p2) <- pairs, let rect = rectangle p1 p2, not (any (intersects rect) linesList)]
   in if null areas then 0 else maximum areas

main :: IO ()
main = do
  input <- getInputData 9
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)
