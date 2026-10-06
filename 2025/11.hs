-- | Day 11: Reactor
--
-- Counts distinct signal routes across directed communication device networks using DFS and memoised state exploration.
module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Utils (getInputData)

-- | Directed graph mapping device names to outgoing neighbouring devices.
type Graph = M.Map String [String]

-- | Parses a line of device connections into a source node and target nodes list.
parseLine :: String -> (String, [String])
parseLine s =
  let (node, rest) = break (== ':') s
      targets = words $ drop 2 rest
  in (node, targets)

-- | Constructs a directed device graph from lines of connection rules.
buildGraph :: [String] -> Graph
buildGraph = M.fromList . map parseLine

-- | Recursively counts all paths from the current node to the destination node.
countPaths :: Graph -> String -> String -> Int
countPaths graph current target
  | current == target = 1
  | otherwise = sum [countPaths graph next target | next <- M.findWithDefault [] current graph]

-- | Memoisation map caching path counts for @(current_node, remaining_required_nodes)@ states.
type Memo = M.Map (String, S.Set String) Int

-- | Traverses the graph with memoisation to count paths visiting all required intermediary nodes.
countPathsMemo :: Graph -> String -> String -> S.Set String -> Memo -> (Int, Memo)
countPathsMemo graph current target required memo
  | current == target =
      if S.null required then (1, memo) else (0, memo)
  | otherwise =
      case M.lookup (current, required) memo of
        Just v -> (v, memo)
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

-- | Solves Part One: counts all valid paths leading from @"you"@ to @"out"@.
part1 :: [String] -> Int
part1 input = countPaths graph "you" "out"
  where
    graph = buildGraph input

-- | Solves Part Two: counts all paths from @"svr"@ to @"out"@ that pass through both @"dac"@ and @"fft"@.
part2 :: [String] -> Int
part2 input =
  let graph = buildGraph input
      required = S.fromList ["dac","fft"]
      (total, _) = countPathsMemo graph "svr" "out" required M.empty
  in total

main :: IO ()
main = do
  input <- getInputData 11
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)
