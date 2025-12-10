module Main where

import Data.Bits (xor, (.|.), shiftL)
import Data.List (foldl', isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (getInputData)

lightsToMask :: String -> Int
lightsToMask s = foldl' (\acc c -> acc * 2 + if c == '#' then 1 else 0) 0 . reverse $ filter (`elem` "#.") s

buttonToMask :: String -> Int
buttonToMask s =
  foldl' (\acc i -> acc .|. (1 `shiftL` i)) 0 indices
  where
    cleaned = map (\c -> if c `elem` ",()" then ' ' else c) s
    indices = map read $ words cleaned

parseLine :: String -> (Int, [Int])
parseLine line =
  case filter ("[" `isPrefixOf`) parts of
    [] -> error "No lights in line"
    (lights:_) ->
      let buttonStrs = filter ("(" `isPrefixOf`) parts
      in (lightsToMask lights, map buttonToMask buttonStrs)
  where
    parts = words line

minPresses :: Int -> [Int] -> Int
minPresses target buttons = bfs (Set.singleton 0) [0] 0
  where
    bfs :: Set Int -> [Int] -> Int -> Int
    bfs visited frontier n
      | target `elem` frontier = n
      | null nextFrontier = error $ "No solution for target " ++ show target
      | otherwise = bfs visited' nextFrontier (n + 1)
      where
        nextFrontier = [m `xor` b | m <- frontier, b <- buttons, not (Set.member (m `xor` b) visited)]
        visited' = foldr Set.insert visited nextFrontier

part1 :: [String] -> Int
part1 = sum . map (uncurry minPresses . parseLine)

part2 :: [String] -> Int
part2 = length

main :: IO ()
main = do
  input <- getInputData 10
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)
