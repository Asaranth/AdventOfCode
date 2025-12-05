module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Utils (getInputData)

parseRange :: String -> (Integer, Integer)
parseRange s =
  case map read (splitOn "-" s) of
    [start, end] -> (start, end)
    _ -> error "Invalid range format"

inRange :: Integer -> (Integer, Integer) -> Bool
inRange x (start, end) = x >= start && x <= end

part1 :: [String] -> [String] -> Int
part1 fresh available =
  let freshRanges = map parseRange fresh
      availableIds = Set.fromList (map read available :: [Integer])
      isFresh x = any (inRange x) freshRanges
   in Set.size (Set.filter isFresh availableIds)

part2 :: [String] -> Integer
part2 fresh = go sortedRanges 0 Nothing
  where
    ranges :: [(Integer, Integer)]
    ranges = map parseRange fresh
    sortedRanges :: [(Integer, Integer)]
    sortedRanges = sortOn fst ranges
    go :: [(Integer, Integer)] -> Integer -> Maybe (Integer, Integer) -> Integer
    go [] total Nothing = total
    go [] total (Just (s, e)) = total + (e - s + 1)
    go ((s, e) : rs) total Nothing = go rs total (Just (s, e))
    go ((s, e) : rs) total (Just (ms, me))
      | s <= me + 1 = go rs total (Just (ms, max me e))
      | otherwise = go rs (total + (me - ms + 1)) (Just (s, e))

main :: IO ()
main = do
  input <- getInputData 5
  let parts = splitOn [""] (lines input)
  let (fresh, available) = case parts of
        [a, b] -> (a, b)
        _ -> error "Expected exactly one blank line separating the sections"
  putStrLn $ "Part One: " ++ show (part1 fresh available)
  putStrLn $ "Part Two: " ++ show (part2 fresh)