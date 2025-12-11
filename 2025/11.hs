module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Utils (getInputData)

-- Type alias for clarity
type Graph = M.Map String [String]

-- Parse a single line like "aaa: you hhh" into ("aaa", ["you","hhh"])
parseLine :: String -> (String, [String])
parseLine s =
  let (node, rest) = break (== ':') s
      targets = words $ drop 2 rest  -- drop ": "
  in (node, targets)

-- Build the graph from all input lines
buildGraph :: [String] -> Graph
buildGraph = M.fromList . map parseLine

-- Count all paths from `current` to `target` in the graph
countPaths :: Graph -> String -> String -> Int
countPaths graph current target
  | current == target = 1
  | otherwise = sum [countPaths graph next target | next <- M.findWithDefault [] current graph]

type Memo = M.Map (String, S.Set String) Int

countPathsMemo :: Graph -> String -> String -> S.Set String -> Memo -> (Int, Memo)
countPathsMemo graph current target required memo
  | current == target =
      if S.null required then (1, memo) else (0, memo)
  | otherwise =
      case M.lookup (current, required) memo of
        Just v -> (v, memo)  -- reuse memoized result
        Nothing ->
          let remaining = if current `S.member` required then S.delete current required else required
              (total, memo') = foldl
                (\(acc, m) next ->
                    let (v, m') = countPathsMemo graph next target remaining m
                    in (acc + v, m'))
                (0, memo)
                (M.findWithDefault [] current graph)
              memo'' = M.insert (current, required) total memo'
          in (total, memo'')

-- Part 1: number of paths from "you" to "out"
part1 :: [String] -> Int
part1 input = countPaths graph "you" "out"
  where
    graph = buildGraph input

part2 :: [String] -> Int
part2 input =
  let graph = buildGraph input
      required = S.fromList ["dac","fft"]
      (total, _) = countPathsMemo graph "svr" "out" required M.empty
  in total



-- Main function
main :: IO ()
main = do
  input <- getInputData 11
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)
