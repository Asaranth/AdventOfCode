-- | Day 04: Printing Department
--
-- Analyses paper roll grid configurations and simulates iterative peeling of cells with low neighbour degrees.
module Main where

import Utils (getInputData)

-- | Relative offsets for all 8 surrounding neighbour cells (horizontal, vertical, and diagonal).
adjacents :: [(Int, Int)]
adjacents = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

-- | Checks whether a row and column coordinate pair falls within the 2D grid boundaries.
inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds grid (r, c) = r >= 0 && r < length grid && c >= 0 && c < length (head grid)

-- | Counts the number of active roll neighbours (@\'\@\'@) surrounding a given grid coordinate.
countNeighbors :: [[Char]] -> (Int, Int) -> Int
countNeighbors grid (r, c) =
  length [() | (dr, dc) <- adjacents, let nr = r + dr, let nc = c + dc, inBounds grid (nr, nc), grid !! nr !! nc == '@']

-- | Finds all roll positions that have fewer than four active neighbours.
accessiblePositions :: [String] -> [(Int, Int)]
accessiblePositions grid =
  [(r, c) | (r, row) <- zip [0 ..] grid, (c, ch) <- zip [0 ..] row, ch == '@', countNeighbors grid (r, c) < 4]

-- | Solves Part One: counts initial roll positions that have fewer than four neighbours.
part1 :: [String] -> Int
part1 grid = length (accessiblePositions grid)

-- | Solves Part Two: iteratively peels away rolls with fewer than four neighbours until none remain.
part2 :: [String] -> Int
part2 grid = go grid 0
  where
    go :: [String] -> Int -> Int
    go g removedTotal =
      let removable = accessiblePositions g
          removedCount = length removable
       in if removedCount == 0
            then removedTotal
            else
              let newGrid = [[if (r, c) `elem` removable then '.' else ch | (c, ch) <- zip [0 ..] row] | (r, row) <- zip [0 ..] g]
               in go newGrid (removedTotal + removedCount)

main :: IO ()
main = do
  input <- getInputData 4
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)