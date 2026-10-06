-- | Day 02: Gift Shop
--
-- Filters and sums IDs across numeric intervals based on symmetry and string periodicity.
module Main where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Utils (getInputData)

-- | Solves the problem by summing IDs from all ranges that satisfy a given predicate.
solve :: (Int -> Bool) -> [String] -> Int
solve predicate ranges = sum [n | r <- ranges, n <- parseRange r, predicate n]

-- | Parses a hyphenated range string into an inclusive list of integers.
parseRange :: String -> [Int]
parseRange s =
  case map read (splitOn "-" s) of
    [start, end] -> [start .. end]
    _ -> error "Invalid range format"

-- | Solves Part One: sums IDs with even digit lengths whose first and second halves are identical.
part1 :: [String] -> Int
part1 = solve $ \n ->
  let s = show n
      len = length s
      half = len `div` 2
      (first, second) = splitAt half s
   in even len && first == second

-- | Solves Part Two: sums IDs whose decimal representations form repeating periodic patterns.
part2 :: [String] -> Int
part2 = solve $ \n ->
  let s = show n
   in s `isInfixOf` init (tail (s ++ s))

main :: IO ()
main = do
  input <- getInputData 2
  let ids = splitOn "," input
  putStrLn $ "Part One: " ++ show (part1 ids)
  putStrLn $ "Part Two: " ++ show (part2 ids)