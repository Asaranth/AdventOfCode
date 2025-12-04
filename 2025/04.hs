module Main where

import Utils (getInputData)

adjacents :: [(Int, Int)]
adjacents = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds grid (r, c) = r >= 0 && r < length grid && c >= 0 && c < length (head grid)

countNeighbors :: [[Char]] -> (Int, Int) -> Int
countNeighbors grid (r, c) =
  length [() | (dr, dc) <- adjacents, let nr = r + dr, let nc = c + dc, inBounds grid (nr, nc), grid !! nr !! nc == '@']

accessiblePositions :: [String] -> [(Int, Int)]
accessiblePositions grid =
  [(r, c) | (r, row) <- zip [0 ..] grid, (c, ch) <- zip [0 ..] row, ch == '@', countNeighbors grid (r, c) < 4]

part1 :: [String] -> Int
part1 grid = length (accessiblePositions grid)

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