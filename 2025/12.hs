module Main where

import Data.List (sum, zipWith)
import Utils (getInputData)

data Region = Region {width :: Int, height :: Int, counts :: [Int]} deriving (Show)

parseInput :: [String] -> ([Int], [Region])
parseInput ls = (shapes, regions)
  where
    (shapeLines, rest) = break (\l -> 'x' `elem` l) ls
    shapeGroups = filter (not . null) $ splitByEmpty shapeLines
    shapes = map countBlocks shapeGroups
    regions = map parseRegion rest

countBlocks :: [String] -> Int
countBlocks lines = length [() | row <- lines, c <- row, c == '#']

parseRegion :: String -> Region
parseRegion line =
  let (dimPart, restCounts) = break (== ':') line
      [w, h] = map read $ splitOn "x" dimPart
      cs = map read $ words (drop 1 restCounts)
   in Region w h cs

splitByEmpty :: [String] -> [[String]]
splitByEmpty [] = []
splitByEmpty xs =
  let (grp, rest) = break null xs
   in grp : splitByEmpty (dropWhile null rest)

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn sep str = case breakList sep str of
  Just (pre, post) -> pre : splitOn sep post
  Nothing -> [str]

breakList :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakList sep xs
  | sep `isPrefixOf` xs = Just ([], drop (length sep) xs)
  | null xs = Nothing
  | otherwise = case breakList sep (tail xs) of
      Just (pre, post) -> Just (head xs : pre, post)
      Nothing -> Nothing

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

fits :: [Int] -> Region -> Bool
fits shapes (Region w h needs) =
  let totalBlocks = sum $ zipWith (*) shapes needs
   in totalBlocks <= w * h

solve :: [String] -> Int
solve ls =
  let (shapes, regions) = parseInput ls
   in length $ filter (fits shapes) regions

main :: IO ()
main = do
  input <- getInputData 12
  let ls = lines input
  putStrLn $ "Solution: " ++ show (solve ls)
