-- | Day 12: Christmas Tree Farm
--
-- Calculates present packing feasibility across bounded farm regions by comparing block area requirements.
module Main where

import Data.List (sum, zipWith)
import Utils (getInputData)

-- | Represents a farm region with dimensions (width, height) and required shape counts.
data Region = Region {width :: Int, height :: Int, counts :: [Int]} deriving (Show)

-- | Parses input lines into shape block sizes and target regional boundary definitions.
parseInput :: [String] -> ([Int], [Region])
parseInput ls = (shapes, regions)
  where
    (shapeLines, rest) = break (\l -> 'x' `elem` l) ls
    shapeGroups = filter (not . null) $ splitByEmpty shapeLines
    shapes = map countBlocks shapeGroups
    regions = map parseRegion rest

-- | Counts the number of solid block tiles (@\'#\'@) within a shape definition.
countBlocks :: [String] -> Int
countBlocks lines = length [() | row <- lines, c <- row, c == '#']

-- | Parses a region string in @\"<w>x<h>: <count1> <count2> ...\"@ format into a 'Region'.
parseRegion :: String -> Region
parseRegion line =
  let (dimPart, restCounts) = break (== ':') line
      [w, h] = map read $ splitOn "x" dimPart
      cs = map read $ words (drop 1 restCounts)
   in Region w h cs

-- | Splits a list of strings into groups separated by empty lines.
splitByEmpty :: [String] -> [[String]]
splitByEmpty [] = []
splitByEmpty xs =
  let (grp, rest) = break null xs
   in grp : splitByEmpty (dropWhile null rest)

-- | Splits a list by a separator sublist.
splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn sep str = case breakList sep str of
  Just (pre, post) -> pre : splitOn sep post
  Nothing -> [str]

-- | Breaks a list at the first occurrence of a sublist separator.
breakList :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakList sep xs
  | sep `isPrefixOf` xs = Just ([], drop (length sep) xs)
  | null xs = Nothing
  | otherwise = case breakList sep (tail xs) of
      Just (pre, post) -> Just (head xs : pre, post)
      Nothing -> Nothing

-- | Checks whether a list starts with a specified prefix list.
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- | Determines whether total present block volume fits within the region area capacity.
fits :: [Int] -> Region -> Bool
fits shapes (Region w h needs) =
  let totalBlocks = sum $ zipWith (*) shapes needs
   in totalBlocks <= w * h

-- | Solves the puzzle by counting how many regions can fit their required present blocks.
solve :: [String] -> Int
solve ls =
  let (shapes, regions) = parseInput ls
   in length $ filter (fits shapes) regions

main :: IO ()
main = do
  input <- getInputData 12
  let ls = lines input
  putStrLn $ "Solution: " ++ show (solve ls)
