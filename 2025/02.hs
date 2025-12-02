module Main where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Utils (getInputData)

solve :: (Int -> Bool) -> [String] -> Int
solve predicate ranges = sum [n | r <- ranges, n <- parseRange r, predicate n]

parseRange :: String -> [Int]
parseRange s =
  case map read (splitOn "-" s) of
    [start, end] -> [start .. end]
    _ -> error "Invalid range format"

part1 :: [String] -> Int
part1 = solve $ \n ->
  let s = show n
      len = length s
      half = len `div` 2
      (first, second) = splitAt half s
   in even len && first == second

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